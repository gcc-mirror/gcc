       *> { dg-do run }
       *> { dg-options "-ffixed-form" }
       *> { dg-output-file "group2/debugging_lines__WITH_DEBUGGING_MODE_.out" }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
           SOURCE-COMPUTER. mine WITH DEBUGGING MODE.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       PROCEDURE        DIVISION.
      *    Original "incorrect ordered lines"
      *    DISPLAY "KO" NO ADVANCING UPON STDOUT
      *    END-DISPLAY.
      D    DISPLAY "KO" UPON STDOUT NO ADVANCING
      D    END-DISPLAY.
           DISPLAY "OK" UPON STDOUT NO ADVANCING
           END-DISPLAY.
           STOP RUN.

