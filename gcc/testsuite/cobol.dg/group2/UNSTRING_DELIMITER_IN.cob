       *> { dg-do run }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       ENVIRONMENT      DIVISION.
       DATA             DIVISION.
       WORKING-STORAGE SECTION.
       01  WK-CMD       PIC X(8) VALUE "WWADDBCC".
       01  WK-SIGNS     PIC XX   VALUE "AB".
       01  WKS REDEFINES WK-SIGNS.
           03 WK-SIGN   PIC X OCCURS 2.
       01  .
         02 WK-DELIM     PIC X OCCURS 2.
       01  .
         02 WK-DATA      PIC X(2) OCCURS 3.
       PROCEDURE        DIVISION.
           UNSTRING WK-CMD DELIMITED BY WK-SIGN(1) OR WK-SIGN(2)
           INTO WK-DATA(1) DELIMITER IN WK-DELIM(1)
                WK-DATA(2) DELIMITER IN WK-DELIM(2)
                WK-DATA(3)
           END-UNSTRING
           IF  WK-DATA(1)   NOT = "WW"
            OR WK-DATA(2)   NOT = "DD"
            OR WK-DATA(3)   NOT = "CC"
            OR WK-DELIM(1)  NOT = "A"
            OR WK-DELIM(2)  NOT = "B"
               DISPLAY """" WK-DATA(1)
                       WK-DATA(2)
                       WK-DATA(3)
                       WK-DELIM(1)
                       WK-DELIM(2) """"
               END-DISPLAY
           END-IF.
           STOP RUN.

