       *> { dg-do run }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       PROCEDURE        DIVISION.
           IF FUNCTION TEST-NUMVAL-F ("+ 1")     NOT = 0
              DISPLAY "Test 1  fail"
              END-DISPLAY
           END-IF.
           IF FUNCTION TEST-NUMVAL-F (" + 1")    NOT = 0
              DISPLAY "Test 2  fail"
              END-DISPLAY
           END-IF.
           IF FUNCTION TEST-NUMVAL-F ("- 1")     NOT = 0
              DISPLAY "Test 3  fail"
              END-DISPLAY
           END-IF.
           IF FUNCTION TEST-NUMVAL-F (" - 1")    NOT = 0
              DISPLAY "Test 4  fail"
              END-DISPLAY
           END-IF.
           IF FUNCTION TEST-NUMVAL-F ("+- 1")    NOT = 2
              DISPLAY "Test 5  fail"
              END-DISPLAY
           END-IF.
           IF FUNCTION TEST-NUMVAL-F ("1 +")     NOT = 3
              DISPLAY "Test 6  fail"
              END-DISPLAY
           END-IF.
           IF FUNCTION TEST-NUMVAL-F ("1 -")     NOT = 3
              DISPLAY "Test 7  fail"
              END-DISPLAY
           END-IF.
           IF FUNCTION TEST-NUMVAL-F ("1 +-")    NOT = 3
              DISPLAY "Test 8  fail"
              END-DISPLAY
           END-IF.
           IF FUNCTION TEST-NUMVAL-F ("1 -+")    NOT = 3
              DISPLAY "Test 9  fail"
              END-DISPLAY
           END-IF.
           IF FUNCTION TEST-NUMVAL-F ("+ 1.1")   NOT = 0
              DISPLAY "Test 10 fail"
              END-DISPLAY
           END-IF.
           IF FUNCTION TEST-NUMVAL-F ("- 1.1")   NOT = 0
              DISPLAY "Test 11 fail"
              END-DISPLAY
           END-IF.
           IF FUNCTION TEST-NUMVAL-F ("1.1 +")   NOT = 5
              DISPLAY "Test 12 fail"
              END-DISPLAY
           END-IF.
           IF FUNCTION TEST-NUMVAL-F ("1.1 -")   NOT = 5
              DISPLAY "Test 13 fail"
              END-DISPLAY
           END-IF.
           IF FUNCTION TEST-NUMVAL-F ("1.1   ")  NOT = 0
              DISPLAY "Test 14 fail"
              END-DISPLAY
           END-IF.
           IF FUNCTION TEST-NUMVAL-F ("1.1   ")  NOT = 0
              DISPLAY "Test 15 fail"
              END-DISPLAY
           END-IF.
           IF FUNCTION TEST-NUMVAL-F ("1.1 -CR") NOT = 5
              DISPLAY "Test 16 fail"
              END-DISPLAY
           END-IF.
           IF FUNCTION TEST-NUMVAL-F ("1.1 E+1") NOT = 0
              DISPLAY "Test 17 fail"
              END-DISPLAY
           END-IF.
           IF FUNCTION TEST-NUMVAL-F ("1.1 E -1") NOT = 0
              DISPLAY "Test 18 fail"
              END-DISPLAY
           END-IF.
           IF FUNCTION TEST-NUMVAL-F ("1.1 EE") NOT = 6
              DISPLAY "Test 19 fail"
              END-DISPLAY
           END-IF.
           IF FUNCTION TEST-NUMVAL-F ("+1.1 E+01") NOT = 0
              DISPLAY "Test 20 fail"
              END-DISPLAY
           END-IF.
           STOP RUN.

