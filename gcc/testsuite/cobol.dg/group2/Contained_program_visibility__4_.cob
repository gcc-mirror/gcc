       *> { dg-do run }
       *> { dg-output-file "group2/Contained_program_visibility__4_.out" }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       PROCEDURE        DIVISION.
           DISPLAY "P1" NO ADVANCING
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
        PROCEDURE        DIVISION.
            DISPLAY "P2" NO ADVANCING
            END-DISPLAY.
            EXIT PROGRAM.
        END PROGRAM prog2.
       END PROGRAM prog.
       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog3.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       PROCEDURE        DIVISION.
           DISPLAY "P3" NO ADVANCING
           END-DISPLAY.
           CALL "prog2"
           END-CALL.
           EXIT PROGRAM.
        IDENTIFICATION   DIVISION.
        PROGRAM-ID.      prog2.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        PROCEDURE        DIVISION.
            DISPLAY "P4" NO ADVANCING
            END-DISPLAY.
            EXIT PROGRAM.
        END PROGRAM prog2.
       END PROGRAM prog3.

