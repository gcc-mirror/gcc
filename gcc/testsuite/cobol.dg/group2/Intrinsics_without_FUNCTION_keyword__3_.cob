       *> { dg-do run }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       ENVIRONMENT      DIVISION.
       CONFIGURATION    SECTION.
       REPOSITORY.
           FUNCTION     PI INTRINSIC
           FUNCTION     E  INTRINSIC.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01  Z            PIC 99V99.
       PROCEDURE        DIVISION.
           MOVE PI TO Z.
           MOVE E TO Z.
           STOP RUN.

