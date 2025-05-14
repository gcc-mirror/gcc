       *> { dg-do run }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01  Y   PIC   S99V9(31).
       PROCEDURE        DIVISION.
           MOVE FUNCTION TAN ( 1.5 ) TO Y.
           IF Y NOT = 14.1014199471717193876460836519877
                   DISPLAY Y
                   END-DISPLAY
           END-IF.
           STOP RUN.

