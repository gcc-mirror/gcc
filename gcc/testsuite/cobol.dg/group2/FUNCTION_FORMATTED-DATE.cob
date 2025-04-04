       *> { dg-do run }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01  str          PIC X(10).
       PROCEDURE        DIVISION.
      *>   Test normal inputs.
           MOVE FUNCTION FORMATTED-DATE ( "YYYYMMDD", 1 ) TO str
           IF str <> "16010101"
              DISPLAY "Test 1 failed: " str END-DISPLAY
           END-IF

           MOVE FUNCTION FORMATTED-DATE ( "YYYY-MM-DD", 1 ) TO str
           IF str <> "1601-01-01"
              DISPLAY "Test 2 failed: " str END-DISPLAY
           END-IF

           MOVE FUNCTION FORMATTED-DATE ( "YYYYDDD", 1 ) TO str
           IF str <> "1601001"
              DISPLAY "Test 3 failed: " str END-DISPLAY
           END-IF

           MOVE FUNCTION FORMATTED-DATE ( "YYYY-DDD", 1 ) TO str
           IF str <> "1601-001"
              DISPLAY "Test 4 failed: " str END-DISPLAY
           END-IF

           MOVE FUNCTION FORMATTED-DATE ( "YYYYWwwD", 1 ) TO str
           IF str <> "1601W011"
              DISPLAY "Test 5 failed: " str END-DISPLAY
           END-IF

           MOVE FUNCTION FORMATTED-DATE ( "YYYY-Www-D", 1 ) TO str
           IF str <> "1601-W01-1"
              DISPLAY "Test 6 failed: " str END-DISPLAY
           END-IF

      *>   Test week number edge cases.
      *>   For 2012-01-01.
           MOVE FUNCTION FORMATTED-DATE ( "YYYYWwwD", 150115 ) TO str
           IF str <> "2011W527"
              DISPLAY "Test 7 failed: " str END-DISPLAY
           END-IF

      *>   and for 2013-12-30.
           MOVE FUNCTION FORMATTED-DATE ( "YYYYWwwD", 150844 ) TO str
           IF str <> "2014W011"
              DISPLAY "Test 8 failed: " str END-DISPLAY
           END-IF

           STOP RUN.

