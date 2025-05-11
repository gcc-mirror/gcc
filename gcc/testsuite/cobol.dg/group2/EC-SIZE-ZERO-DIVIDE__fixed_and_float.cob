       *> { dg-do run }
       *> { dg-output-file "group2/EC-SIZE-ZERO-DIVIDE__fixed_and_float.out" }

       IDENTIFICATION DIVISION.
       PROGRAM-ID. prog.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  x PIC 9 VALUE 0.
       01  y PIC 9 VALUE 0.
       01  fx comp-2 VALUE 0.
       01  fy comp-2 VALUE 0.
       PROCEDURE DIVISION.
           DISPLAY "Fixed-point divide by zero:"
           DIVIDE x BY y GIVING y
           DISPLAY "1 - """ FUNCTION TRIM(FUNCTION EXCEPTION-STATUS) """"
           IF FUNCTION TRIM(FUNCTION EXCEPTION-STATUS)
           NOT = 'EC-SIZE-ZERO-DIVIDE'
              DISPLAY '1 Wrong/missing exception: '
                      FUNCTION EXCEPTION-STATUS
              END-DISPLAY
           END-IF
           SET LAST EXCEPTION TO OFF
           DISPLAY "2 - """ FUNCTION TRIM(FUNCTION EXCEPTION-STATUS) """"
           IF FUNCTION EXCEPTION-STATUS NOT = SPACES
              DISPLAY '2 Exception is not empty after reset: '
                      FUNCTION EXCEPTION-STATUS
              END-DISPLAY
           END-IF
           MOVE 0 TO y
           COMPUTE y = x - 1 / y + 6.5
           DISPLAY "3 - """ FUNCTION TRIM(FUNCTION EXCEPTION-STATUS) """"
           IF FUNCTION TRIM(FUNCTION EXCEPTION-STATUS)
           NOT = 'EC-SIZE-ZERO-DIVIDE'
              DISPLAY '3 Wrong/missing exception: '
                      FUNCTION EXCEPTION-STATUS
              END-DISPLAY
           END-IF.
           SET LAST EXCEPTION TO OFF
           DISPLAY "Floating-point divide by zero:"
           DIVIDE fx BY fy GIVING fy
           DISPLAY "4 - """ FUNCTION TRIM(FUNCTION EXCEPTION-STATUS) """"
           IF FUNCTION TRIM(FUNCTION EXCEPTION-STATUS)
           NOT = 'EC-SIZE-ZERO-DIVIDE'
              DISPLAY '4 Wrong/missing exception: '
                      FUNCTION EXCEPTION-STATUS
              END-DISPLAY
           END-IF
           SET LAST EXCEPTION TO OFF
           DISPLAY "5 - """ FUNCTION TRIM(FUNCTION EXCEPTION-STATUS) """"
           IF FUNCTION EXCEPTION-STATUS NOT = SPACES
              DISPLAY '5 Exception is not empty after reset: '
                      FUNCTION EXCEPTION-STATUS
              END-DISPLAY
           END-IF
           MOVE 0 TO fy
           COMPUTE fy = fx - 1 / fy + 6.5
           DISPLAY "6 - """ FUNCTION TRIM(FUNCTION EXCEPTION-STATUS) """"
           IF FUNCTION TRIM(FUNCTION EXCEPTION-STATUS)
           NOT = 'EC-SIZE-ZERO-DIVIDE'
              DISPLAY '6 Wrong/missing exception: '
                      FUNCTION EXCEPTION-STATUS
              END-DISPLAY
           END-IF.

