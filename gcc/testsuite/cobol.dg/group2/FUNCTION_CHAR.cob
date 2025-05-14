       *> { dg-do run }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01  X            PIC   S9(4)V9(4) VALUE 108.
       01  TEST-FLD.
           05  TEST-DATA  PIC X(01).
               88  VALID-DATA   VALUE 'k'.
           05  TEST-UNSET PIC X VALUE '_'.
               88  VALID-UNSET  VALUE '_'.
       PROCEDURE        DIVISION.
           STRING FUNCTION CHAR ( X )
                  DELIMITED BY SIZE
                  INTO TEST-FLD
           END-STRING.
           EVALUATE TRUE
              WHEN NOT VALID-UNSET
                 DISPLAY "FUNCTION result too long"
                 END-DISPLAY
              WHEN VALID-DATA
                 CONTINUE
              WHEN OTHER
                 DISPLAY TEST-DATA
                 END-DISPLAY
           END-EVALUATE.
           STOP RUN.

