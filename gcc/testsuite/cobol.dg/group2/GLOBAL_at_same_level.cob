       *> { dg-do run }
       *> { dg-output-file "group2/GLOBAL_at_same_level.out" }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  X   PIC X(5) GLOBAL  VALUE "prog1".
       PROCEDURE        DIVISION.
           DISPLAY X
           END-DISPLAY.
           CALL "prog2"
           END-CALL
           CALL "prog3"
           END-CALL
           STOP RUN.
        IDENTIFICATION   DIVISION.
        PROGRAM-ID.      prog2.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01  X   PIC X(5) GLOBAL  VALUE "prog2".
        PROCEDURE        DIVISION.
            DISPLAY X
            END-DISPLAY.
            EXIT PROGRAM.
        END PROGRAM prog2.
        IDENTIFICATION   DIVISION.
        PROGRAM-ID.      prog3.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        PROCEDURE        DIVISION.
            DISPLAY X
            END-DISPLAY.
            EXIT PROGRAM.
        END PROGRAM prog3.
       END PROGRAM prog.

