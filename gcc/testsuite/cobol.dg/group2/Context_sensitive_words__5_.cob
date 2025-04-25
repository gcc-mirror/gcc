       *> { dg-do run }
       *> { dg-output-file "group2/Context_sensitive_words__5_.out" }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog RECURSIVE.
       ENVIRONMENT      DIVISION.
       CONFIGURATION    SECTION.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01  RECURSIVE    PIC 9 VALUE 0.
       PROCEDURE        DIVISION.
           DISPLAY RECURSIVE NO ADVANCING
           END-DISPLAY.
           STOP RUN.

