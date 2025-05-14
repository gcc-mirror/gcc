       *> { dg-do run }
       *> { dg-output-file "group2/debugging_lines__not_active_.out" }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       PROCEDURE        DIVISION.
           DISPLAY "OK" NO ADVANCING
           END-DISPLAY.
      D    DISPLAY "KO" NO ADVANCING
      D    END-DISPLAY.
           STOP RUN.

