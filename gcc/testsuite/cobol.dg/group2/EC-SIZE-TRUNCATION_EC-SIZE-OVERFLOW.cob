       *> { dg-do run }
       *> { dg-output-file "group2/EC-SIZE-TRUNCATION_EC-SIZE-OVERFLOW.out" }

       IDENTIFICATION DIVISION.
       PROGRAM-ID. prog.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  x PIC 9 VALUE 1.
       01  y PIC 9.
       01  a     COMP-1 VALUE 1.E20.
       01  b     COMP-1 VALUE 1.E20.
       PROCEDURE DIVISION.
           DIVIDE x BY 0.1 GIVING y
           DISPLAY FUNCTION TRIM(FUNCTION EXCEPTION-STATUS)
           IF FUNCTION TRIM(FUNCTION EXCEPTION-STATUS)
           NOT = 'EC-SIZE-TRUNCATION'
              DISPLAY 'Wrong/missing exception: '
                      FUNCTION EXCEPTION-STATUS
              END-DISPLAY
           END-IF.
        SET LAST EXCEPTION TO OFF
           MULTIPLY a BY b GIVING b
           DISPLAY FUNCTION TRIM(FUNCTION EXCEPTION-STATUS)
           IF FUNCTION TRIM(FUNCTION EXCEPTION-STATUS)
           NOT = 'EC-SIZE-OVERFLOW'
              DISPLAY 'Wrong/missing exception: '
                      FUNCTION EXCEPTION-STATUS
              END-DISPLAY
           END-IF.

