       *> { dg-do run }
       *> { dg-output-file "group2/Index_and_parenthesized_expression.out" }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01 G.
         02 X           PIC X OCCURS 1 INDEXED BY I.
       PROCEDURE        DIVISION.
         IF I < (I + 2)
           DISPLAY "OK" NO ADVANCING
           END-DISPLAY
         END-IF.
         STOP RUN.

