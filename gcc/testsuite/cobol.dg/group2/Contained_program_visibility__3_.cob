       *> { dg-do run }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  X   PIC X(5) GLOBAL  VALUE "prog1".
       PROCEDURE        DIVISION.
           IF X NOT = "prog1"
              DISPLAY X
              END-DISPLAY
           END-IF.
           CALL "prog2"
           END-CALL.
           STOP RUN.
        IDENTIFICATION   DIVISION.
        PROGRAM-ID.      prog2.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01  X   PIC X(5) GLOBAL  VALUE "prog2".
        PROCEDURE        DIVISION.
            IF X NOT = "prog2"
               DISPLAY X
               END-DISPLAY
            END-IF.
            CALL "prog3"
            END-CALL.
            EXIT PROGRAM.
        END PROGRAM prog2.
        IDENTIFICATION   DIVISION.
        PROGRAM-ID.      prog3 COMMON.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        PROCEDURE        DIVISION.
            IF X NOT = "prog1"
               DISPLAY X
               END-DISPLAY
            END-IF.
            EXIT PROGRAM.
        END PROGRAM prog3.
       END PROGRAM prog.

