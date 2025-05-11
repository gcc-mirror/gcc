       *> { dg-do run }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01 G             VALUE "d4b2e1a3c5".
         02 TBL         OCCURS 5.
           03 X         PIC X.
           03 Y         PIC 9.
       PROCEDURE        DIVISION.
           SORT TBL ASCENDING KEY X.
           IF G NOT = "a3b2c5d4e1"
              DISPLAY G
              END-DISPLAY
           END-IF.
           SORT TBL DESCENDING KEY Y.
           IF G NOT = "c5d4a3b2e1"
              DISPLAY G
              END-DISPLAY
           END-IF.
           SORT TBL ASCENDING KEY TBL.
           IF G NOT = "a3b2c5d4e1"
              DISPLAY G
              END-DISPLAY
           END-IF.
           SORT TBL DESCENDING KEY.
           IF G NOT = "e1d4c5b2a3"
              DISPLAY G
              END-DISPLAY
           END-IF.
           STOP RUN.

