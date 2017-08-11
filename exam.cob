       IDENTIFICATION PROGRAM.
       PROGRAM-ID. EXAM.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-COM
           02 WS-CA         PIC X(01) VALUE 1.
           02 WS-EMP-ID     PIC X(04).
       01 WS-RESP-CODE      PIC S9(08) COMP VALUE ZERO.
       01 MASTER-RECORD.
           02 M-ID          
               03 M-ID-1    PIC X(01).
               03 M-ID-2    PIC X(03).
           02 M-NAME        PIC X(15).
           02 M-ADDR        PIC X(03).
           02 M-DEPT        PIC X(04).
           02 M-DOB.
               03 D-DD      PIC 9(02).
               03 FILLER    PIC X(01) VALUE ':'.
               03 D-MM      PIC 9(02).
               03 FILLER    PIC X(01) VALUE ':'
               03 D-YY      PIC 9(04).
           02 M-JOIN.
               03 J-DD      PIC 9(02).
               03 FILLER    PIC X(01) VALUE ':'.
               03 J-MM      PIC 9(02).
               03 FILLER    PIC X(01) VALUE ':'
               03 J-YY      PIC 9(04).      
       01 FLAG              PIC X(01) VALUE 'N'.
       LINKAGE SECTION.
       01 DFHCOMMAREA       PIC X(02).
       PROCEDURE DIVISION.
       MAIN-PARA.
           IF EIBCALEN = 0 
               PERFORM FIRST-PARA
           ELSE
               MOVE DFCOMMAREA TO WS-COM
               PERFORM NEXT-PARA
           END-IF.
       FIRST-PARA.
           MOVE LOW-VALUES TO MAP1O , MAP2O
           PERFORM SEND-MAP-1.
       NEXT-PARA.
           EVALUATE EIBAID
               WHEN DFHENTER
                   IF WS-CA = 1
                       PERFORM ENTER-PARA
                   ELSE
                       MOVE ' INVALID KEY PRESSED ' TO ERRORO
                   END-IF.
               WHEN DFHPF5
                   IF WS-CA = 1
                       PERFORM REFRESH-PARA
                   ELSE
                       MOVE ' INVALID KEY ' TO DERRORO
                   END-IF.
               WHEN DFHPF3
                   IF WS-CA = 1
                       PERFORM EXIT-PARA
                   ELSE
                       PERFORM SEND-MAP-1
                   END-IF.
               WHEN DFHPF2
                   IF WS-CA = 1
                       PERFORM ADD-PARA
                   ELSE
                       PERFORM MODIFY-PARA
                   END-IF.
               WHEN DFHPF4
                   IF WS-CA = 2
                       PERFORM DELETE-PARA
                   ELSE
                       MOVE ' CANNOT DELETE HERE ' TO DERRORO
                   END-IF.
               WHEN OTHER
                   MOVE ' PLEASE CHECK THE OPTION ' TO ERRORO , DERRORO
           END-EVALUATE.
       ENTER-PARA.
           PERFORM RECEIVE-MAP-1
           MOVE IDI TO M-ID
           IF IDI = SPACES 
               MOVE 'ENTER THE EMP ID ' TO ERRORO
               PERFORM SEND-MAP-1
           ELSE
               PERFORM READ-PARA
               PEROFRM SEND-MAP-2
           END-IF.
       READ-PARA.
           MOVE LOW-VALUE TO MAP20
           MOVE IDI TO WS-EMP-ID
           EXEC CICS READ
               FILE('P10BFILE')
               RIDFLD(WS-EMP-ID)
               INTO(MASTER-RECORD)                                       
               RESP(WS-RESP-CODE)
           END-EXEC. 
           EVALUATE WS-RESP-CODE
               WHEN DFHRESP(NORMAL)
                   MOVE 'MODIFY' TO LABEL1O
                   MOVE 2 TO WS-CA
                   PERFORM MOVE-PARA
               WHEN DFHRESP(NOTFND)
                   MOVE ' ADD ' TO LABEL1O
                   MOVE DFHBMDAR TO LABEL2
                   MOVE 1 TO WS-CA
                   MOVE WS-EMP-ID TO DIDO
               WHEN OTHER
                   MOVE ' ERROR ACCESSING FILE ' TO DERRORO
           END-EVALUATE.
       MOVE-PARA.
           MOVE M-ID TO DIDO
           MOVE M-NAME TO DNAMEO
           MOVE M-ADDR TO DADDRO
           MOVE M-DEPT TO DDEPTO
           MOVE M-DOB TO DDOBO
           MOVE M-JOIN TO DJOINO.
       MODIFY-PARA.
           PERFORM RECEIVE-MAP-2                                         
           PERFORM VALIDATE-PARA
           IF FLAG = 'Y'
               PEROFRM REWRITE-PARA
           END-IF.
       REWRITE-PARA.
           EXEC CICS READ
               FILE('P10BFILE')
               RIDFLD(WS-EMP-ID)
               INTO(MASTER-RECORD)                                       
               UPDATE
           END-EXEC
           EXEC CICS REWRITE
               FILE('P10BFILE')
               FROM(MASTER-RECORD)
               RESP(WS-RESP-CODE)
           END-EXEC
           EVALUATE WS-RESP-CODE        
               WHEN DFHRESP(NORMAL)
                   MOVE ' RECORD UPDATED ' TO ERRORO
                   PERFORM SEND-PARA-1
               WHEN OTHER
                   MOVE ' RECORD NOT UPDATED ' TO ERRORO       
                   PERFORM SEND-PARA-1
           END-EVALUATE.
       ADD-PARA.
           PERFORM RECEIVE-MAP-2
           PERFORM VALIDATE-PARA
           IF FLAG = 'Y'
               PERFORM WRITE-PARA
           END-IF.
       WRITE-PARA.
           PERFORM A-MOVE-PARA
           EXEC CICS WRITE
               FILE('P10BFILE')
               RIDFLD(WS-EMP-ID)
               FROM(MASTER-RECORD)
               RESP(WS-RESP-CODE)
           END-EXEC
           EVALUATE WS-RESP-CODE    
               WHEN DFHRESP(NORMAL)
                   MOVE ' RECORD ADDED' TO ERRORO
                   PERFORM SEND-MAP-1
               WHEN OTHER
                   MOVE ' RECORD NOT ADDED ' TO DERRORO
                   PERFORM SEND-PARA-2
           END-EVALUATE.    
       DELETE-PARA.
           EXEC CICS READ
               FILE('P10BFILE')
               RIDFLD(WS-EMP-ID)
               INTO(MASTER-RECORD)
               UPDATE
           END-EXEC
           EXEC CICS DELETE
               FILE('P10BFILE')
           END-EXEC.
       A-MOVE-PARA.
           MOVE DIDI TO M-ID
           MOVE DNAMEI TO M-NAME
           MOVE DADDR TO M-ADDR
           MOVE DDEPTI TO M-DEPT
           MOVE DDOBI TO M-DOB
           MOVE DJOIN TO M-JOIN.
       SEND-PARA-1.
           EXEC CICS SEND 
               MAP('MAP1')
               MAPSET('P10AS09')
               FROM(MAP1O)
               CURSOR
           END-EXEC
           EXEC CICS RETURN
               TRANSID(P10I)
               COMMAREA(WS-COM)
           END-EXEC.
       SEND-PARA-2.
           EXEC CICS SEND 
               MAP('MAP2')
               MAPSET('P10AS09')
               FROM(MAP2O)
               CURSOR
           END-EXEC
           EXEC CICS RETURN
               TRANSID(P10I)
               COMMAREA(WS-COM)
           END-EXEC.
       RECEIVE-PARA-1.   
           EXEC CICS RECEIVE
               MAP('MAP1')
               MAPSET('P10AS09')
               FROM(MAP1I)
           END-EXEC.
       RECEIVE-PARA-2.   
           EXEC CICS RECEIVE
               MAP('MAP2')
               MAPSET('P10AS09')
               FROM(MAP2I)
           END-EXEC.
       VALIDATE-PARA.
           IF M-ID-1 = ZERO 
               MOVE -1 TO DIDL
               MOVE 'N' TO FLAG
           END-IF 
           IF M-NAME = ALPHABETIC 
               MOVE -1 TO DNAMEL
               MOVE 'N' TO FLAG
           END-IF 
           IF D-YY = ZERO OR D-YY > WS-YY 
               MOVE -1 TO DDOBL
               MOVE 'N' TO FLAG
           END-IF 
           IF D-MM > 0 AND D-MM < 12
               IF D-MM = 1 OR D-MM = 3 OR D-MM = 5 OR 
                  D-MM = 7 OR D-MM = 8 OR D-MM = 10 OR D-MM = 12
                   IF D-DD > 0 AND D-DD < 32
                       MOVE 'Y' TO FLAG
                   ELSE
                       MOVE 'N' TO FLAG
                       MOVE -1 TO DDOBL
                   END-IF
               IF D-MM = 2 
                   IF D-DD >0 AND D-DD < 29 
                       MOVE 'Y' TO FLAG
                   ELSE
                       MOVE 'N' TO FLAG
                       MOVE -1 TO DDOBL
                   END-IF
               ELSE
                   IF D-DD > 0 AND D-DD < 31
                       MOVE 'Y' TO FLAG
                   ELSE
                       MOVE'N' TO FLAG
                       MOVE -1 TO DDOBL
                   END-IF
               END-IF.
           ELSE
               MOVE 'N' TO FLAG
               MOVE -1 TO DDOBL
           END-IF          
           IF J-MM > 0 AND J-MM < 12
               IF J-MM = 1 OR J-MM = 3 OR J-MM = 5 OR 
                  J-MM = 7 OR J-MM = 8 OR J-MM = 10 OR J-MM = 12
                   IF J-DD > 0 AND J-DD < 32
                       MOVE 'Y' TO FLAG
                   ELSE
                       MOVE 'N' TO FLAG
                       MOVE -1 TO DJOINL
                   END-IF
               IF J-MM = 2 
                   IF J-DD >0 AND J-DD < 29 
                       MOVE 'Y' TO FLAG
                   ELSE
                       MOVE 'N' TO FLAG
                       MOVE -1 TO DJOINL
                   END-IF
               ELSE
                   IF J-DD > 0 AND J-DD < 31
                       MOVE 'Y' TO FLAG
                   ELSE
                       MOVE'N' TO FLAG
                       MOVE -1 TO DJOINL
                   END-IF
               END-IF.
           ELSE
               MOVE 'N' TO FLAG
               MOVE -1 TO DJOINL
           END-IF
           IF J-YY = ZERO OR J-YY > WS-YY 
               MOVE -1 TO DJOINL
               MOVE 'N' TO FLAG
           END-IF 
           IF DTECH = 'SALES' OR DTECH = 'ADMIN' OR DTECH = 'HR' OR 
                   DTECH = 'TRAINING'         
               MOVE 'Y' TO FLAG
           ELSE
               MOVE 'N' TO FLAG
               MOVE -1 TO DTECHL
           END-IF
           IF (WS-YY - D-YY) > 18
               IF J-YY < D-YY
                   MOVE 'Y' TO FLAG
               ELSE
                   MOVE 'N' TO FLAG
                   MOVE -1 TO DJOINL
               END-IF
           ELSE
               MOVE 'N' TO FLAG
               MOVE -1 TO DJOINL
           END-IF.
       DATE-TIME-PARA.
           EXEC CICS ASKTIME
               ABSTIME(WS-DATE-TIME)
           END-EXEC
           EXEC CICS FORMATTIME
               ABSTIME(WS-DATE-TIME)
               DDMMYYYY(DATEO)
               DATESEP
           END-EXEC
           MOVE DATEO TO DDATEO.
