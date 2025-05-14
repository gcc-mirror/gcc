       *> { dg-do run }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01  X            PIC X(10) VALUE "A#B.C%D+E$".
       01  TEST-FLD     PIC X(12) VALUE ALL '_'.
       PROCEDURE        DIVISION.
           STRING FUNCTION LOWER-CASE ( X )
                  DELIMITED BY SIZE
                  INTO TEST-FLD
           END-STRING
           IF TEST-FLD NOT = 'a#b.c%d+e$__'
              DISPLAY TEST-FLD
              END-DISPLAY
           END-IF.
           STOP RUN.

