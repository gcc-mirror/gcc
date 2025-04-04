       *> { dg-do run }
       *> { dg-output-file "group2/FUNCTION_NUMVAL-F.out" }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01   result      PIC S9(8)V9(9) COMP-5.
       01   vector.
        05  vd.
          10  FILLER   PIC  X(32)  VALUE   " - 123.456 E + 2 ".
          10  FILLER   PIC  X(32)  VALUE   "123".
          10  FILLER   PIC  X(32)  VALUE   ".456".
          10  FILLER   PIC  X(32)  VALUE   "123.456".
          10  FILLER   PIC  X(32)  VALUE   "-123.456".
          10  FILLER   PIC  X(32)  VALUE   "123.456E2".
          10  FILLER   PIC  X(32)  VALUE   "-123.456E-2".
          10  FILLER   PIC  X(32)  VALUE   "DONE".
          10  FILLER   PIC  X(32)  OCCURS 100 TIMES.
        05  datat REDEFINES vd PIC X(32) OCCURS 100 TIMES INDEXED BY I.
       PROCEDURE        DIVISION.
            SET I TO 1
            PERFORM UNTIL datat(I) EQUALS "DONE"
                DISPLAY """"datat(I)"""" SPACE WITH NO ADVANCING
                MOVE FUNCTION NUMVAL-F(datat(I)) TO result
                DISPLAY result
                ADD 1 TO I
                END-PERFORM.
            STOP RUN.

