       *> { dg-do run }
       *> { dg-xfail-run-if "" { *-*-* }  }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       PROCEDURE        DIVISION.
           STOP RUN WITH ERROR STATUS.

