       *> { dg-do run }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
          1 COMPUTE-DATA.
           2 COMPUTE-8             PICTURE 999       VALUE ZERO.
       PROCEDURE        DIVISION.
           COMPUTE COMPUTE-8 = (((24.0 + 1) * (60 - 10)) / 125) ** 2
           IF COMPUTE-8 NOT = 100
              DISPLAY 'COMPUTE with wrong result: ' COMPUTE-8
              END-DISPLAY
           END-IF
           COMPUTE COMPUTE-8 = 55 / (1 - 2 + 1)
              NOT ON SIZE ERROR
                 DISPLAY 'SIZE ERROR not set from divide by zero!'
                 END-DISPLAY
           END-COMPUTE
           COMPUTE COMPUTE-8 = 0 ** 1
           IF COMPUTE-8 NOT = 0
              DISPLAY '0 ** 1 <> 0: ' COMPUTE-8
              END-DISPLAY
           END-IF
           COMPUTE COMPUTE-8 = 55 ** 0
           IF COMPUTE-8 NOT = 1
              DISPLAY '55 ** 0 <> 1: ' COMPUTE-8
              END-DISPLAY
           END-IF
           COMPUTE COMPUTE-8 = 1 ** 55
           IF COMPUTE-8 NOT = 1
              DISPLAY '11 ** 55 <> 1: ' COMPUTE-8
              END-DISPLAY
           END-IF

           GOBACK.

