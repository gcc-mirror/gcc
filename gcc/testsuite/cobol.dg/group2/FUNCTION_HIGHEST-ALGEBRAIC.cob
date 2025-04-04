       *> { dg-do run }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01  F1           PIC S999.
       01  F2           PIC S9(4) BINARY.
       01  F3           PIC 99V9(3).
       01  F4           PIC $**,**9.99BCR.
       01  F5           PIC $**,**9.99.
       01  F6           USAGE BINARY-CHAR SIGNED.
       01  F7           USAGE BINARY-CHAR UNSIGNED.
       01  F8           PIC 999PPP.
       01  F9           PIC P99.
       01  TEST-FLD     PIC S9(08)V9(04).
       PROCEDURE        DIVISION.
           MOVE FUNCTION HIGHEST-ALGEBRAIC (F1)
             TO TEST-FLD.
           IF TEST-FLD NOT = 999
              DISPLAY "Test 1 fail: " TEST-FLD
              END-DISPLAY
           END-IF.
           MOVE FUNCTION HIGHEST-ALGEBRAIC (F2)
             TO TEST-FLD.
           IF TEST-FLD NOT = 9999
              DISPLAY "Test 2 fail: " TEST-FLD
              END-DISPLAY
           END-IF.
           MOVE FUNCTION HIGHEST-ALGEBRAIC (F3)
             TO TEST-FLD.
           IF TEST-FLD NOT = 99.999
              DISPLAY "Test 3 fail: " TEST-FLD
              END-DISPLAY
           END-IF.
           MOVE FUNCTION HIGHEST-ALGEBRAIC (F4)
             TO TEST-FLD.
           IF TEST-FLD NOT = 99999.99
              DISPLAY "Test 4 fail: " TEST-FLD
              END-DISPLAY
           END-IF.
           MOVE FUNCTION HIGHEST-ALGEBRAIC (F5)
             TO TEST-FLD.
           IF TEST-FLD NOT = 99999.99
              DISPLAY "Test 5 fail: " TEST-FLD
              END-DISPLAY
           END-IF.
           MOVE FUNCTION HIGHEST-ALGEBRAIC (F6)
             TO TEST-FLD.
           IF TEST-FLD NOT = 127
              DISPLAY "Test 6 fail: " TEST-FLD
              END-DISPLAY
           END-IF.
           MOVE FUNCTION HIGHEST-ALGEBRAIC (F7)
             TO TEST-FLD.
           IF TEST-FLD NOT = 255
              DISPLAY "Test 7 fail: " TEST-FLD
              END-DISPLAY
           END-IF.

           MOVE FUNCTION HIGHEST-ALGEBRAIC (F8)
             TO TEST-FLD.
           IF TEST-FLD NOT = 999000
              DISPLAY "Test 7 fail: " TEST-FLD
              END-DISPLAY
           END-IF.

           MOVE FUNCTION HIGHEST-ALGEBRAIC (F9)
             TO TEST-FLD.
           IF TEST-FLD NOT = 0.099
              DISPLAY "Test 7 fail: " TEST-FLD
              END-DISPLAY
           END-IF.

           STOP RUN.

