       *> { dg-do run }
       *> { dg-output-file "group2/Context_sensitive_words__4_.out" }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       ENVIRONMENT      DIVISION.
       CONFIGURATION    SECTION.
       REPOSITORY.
           FUNCTION ALL INTRINSIC.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01  INTRINSIC    PIC 9 VALUE 0.
       PROCEDURE        DIVISION.
           DISPLAY INTRINSIC NO ADVANCING
           END-DISPLAY.
           STOP RUN.

