       *> { dg-do run }
       *> { dg-output-file "group2/Hexadecimal_literal.out" }

        >>DEFINE CHARSET AS 'ASCII'
       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       PROCEDURE        DIVISION.
       >>IF CHARSET = 'EBCDIC'
           DISPLAY X"F1F2F3"
       >>ELSE
           DISPLAY X"313233"
       >>END-IF
           END-DISPLAY.
           STOP RUN.

