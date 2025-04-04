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
       01  F8           PIC S999PPP.
       01  F9           PIC SP99.
       PROCEDURE        DIVISION.
           IF FUNCTION LOWEST-ALGEBRAIC (F1) NOT = -999
              DISPLAY "Test 1 fail"
              END-DISPLAY
           END-IF.
           IF FUNCTION LOWEST-ALGEBRAIC (F2) NOT = -9999
              DISPLAY "Test 2 fail"
              END-DISPLAY
           END-IF.
           IF FUNCTION LOWEST-ALGEBRAIC (F3) NOT = 0
              DISPLAY "Test 3 fail"
              END-DISPLAY
           END-IF.
           IF FUNCTION LOWEST-ALGEBRAIC (F4) NOT = -99999.99
              DISPLAY "Test 4 fail"
              END-DISPLAY
           END-IF.
           IF FUNCTION LOWEST-ALGEBRAIC (F5) NOT = 0
              DISPLAY "Test 5 fail"
              END-DISPLAY
           END-IF.
           IF FUNCTION LOWEST-ALGEBRAIC (F6) NOT = -128
              DISPLAY "Test 6 fail"
              END-DISPLAY
           END-IF.
           IF FUNCTION LOWEST-ALGEBRAIC (F7) NOT = 0
              DISPLAY "Test 7 fail"
              END-DISPLAY
           END-IF.
           IF FUNCTION LOWEST-ALGEBRAIC (F8) NOT = -999000
              DISPLAY "Test 8 fail"
              END-DISPLAY
           END-IF.
           IF FUNCTION LOWEST-ALGEBRAIC (F9) NOT = -0.099
              DISPLAY "Test 9 fail"
              END-DISPLAY
           END-IF.

           STOP RUN.

