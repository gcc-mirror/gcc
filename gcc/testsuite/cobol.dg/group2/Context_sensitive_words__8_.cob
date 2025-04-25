       *> { dg-do run }
       *> { dg-output-file "group2/Context_sensitive_words__8_.out" }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       ENVIRONMENT      DIVISION.
       CONFIGURATION    SECTION.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01  UNBNDED.
           03 ATTRIBUTES  PIC 9 VALUE 0.
       01  LOC.
           03 NAMESPACE   PIC 9 VALUE 1.
       PROCEDURE        DIVISION.
           DISPLAY UNBNDED ATTRIBUTES
                   NAMESPACE IN LOC
                   NO ADVANCING.
           STOP RUN.

