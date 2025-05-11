       *> { dg-do run }
       *> { dg-output-file "group2/ANY_LENGTH__2_.out" }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      caller.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01 P1            PIC X(2) VALUE "OK".
       PROCEDURE        DIVISION.
           CALL "callee" USING P1
           END-CALL.
           DISPLAY "On return, P1 is " """" P1 """"
           STOP RUN.
       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      callee.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01 P2            PIC XXX.
       LINKAGE          SECTION.
       01 P1            PIC X ANY LENGTH.
       PROCEDURE        DIVISION USING P1.
           MOVE P1 TO P2.
           DISPLAY "P1 is " """" P1 """"
           DISPLAY "P2 is " """" P2 """"
           IF P2 NOT = "OK "
              DISPLAY P2
              END-DISPLAY
           END-IF.
           MOVE SPACE TO P1.
           EXIT PROGRAM.
       END PROGRAM callee.
       END PROGRAM caller.

