       *> { dg-do run }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01  VAR          COMP-2 VALUE 0.0.

       PROCEDURE        DIVISION.
           MOVE 9.899999999999E+304 TO VAR
           IF VAR < 0
               DISPLAY "error: compare " VAR " < " 0 " failed!"
               END-DISPLAY
           END-IF.
           IF VAR < 9.799999999999E+304
               DISPLAY 'error: compare ' VAR ' < ' 9.799999999999E+304
                       ' failed!'
               END-DISPLAY
           END-IF.
           IF VAR > 9.999999999999E+304
               DISPLAY 'error: compare ' VAR ' > ' 9.999999999999E+304
                       ' failed!'
               END-DISPLAY
           END-IF.
           MOVE -9.899999999999E+304 TO VAR
           IF VAR > 0
               DISPLAY 'error: compare ' VAR ' > ' 0
                       ' failed!'
               END-DISPLAY
           END-IF.
           IF VAR < -9.999999999999E+304
               DISPLAY 'error: compare ' VAR ' < ' -9.999999999999E+304
                       ' failed!'
               END-DISPLAY
           END-IF.
           IF VAR > -9.799999999999E+304
               DISPLAY 'error: compare ' VAR ' > ' -9.799999999999E+304
                       ' failed!'
               END-DISPLAY
           END-IF.

           STOP RUN.

