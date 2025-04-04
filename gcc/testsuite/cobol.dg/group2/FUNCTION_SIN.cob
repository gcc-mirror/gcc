       *> { dg-do run }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01  Y   PIC   S9V9(33).
       PROCEDURE        DIVISION.
           MOVE FUNCTION SIN ( 1.5 ) TO Y.
           IF Y NOT = 0.997494986604054430941723371141487
                   DISPLAY Y
                   END-DISPLAY
           END-IF.
           STOP RUN.

