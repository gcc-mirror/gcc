       *> { dg-do run }

        IDENTIFICATION   DIVISION.
        PROGRAM-ID.      prog.
        DATA             DIVISION.
        WORKING-STORAGE  SECTION.
        PROCEDURE        DIVISION.
            IF FUNCTION TEST-FORMATTED-DATETIME
                   ("YYYYMMDD", "16010101") <> 0
                DISPLAY "Test 1 failed" END-DISPLAY
            END-IF
            IF FUNCTION TEST-FORMATTED-DATETIME
                   ("YYYY-MM-DD", "1601-01-01") <> 0
                DISPLAY "Test 2 failed" END-DISPLAY
            END-IF
            IF FUNCTION TEST-FORMATTED-DATETIME
                   ("YYYYDDD", "1601001") <> 0
                DISPLAY "Test 3 failed" END-DISPLAY
            END-IF
            IF FUNCTION TEST-FORMATTED-DATETIME
                   ("YYYY-DDD", "1601-001") <> 0
                DISPLAY "Test 4 failed" END-DISPLAY
            END-IF
            IF FUNCTION TEST-FORMATTED-DATETIME
                   ("YYYYWwwD", "1601W011") <> 0
                DISPLAY "Test 5 failed" END-DISPLAY
            END-IF
            IF FUNCTION TEST-FORMATTED-DATETIME
                   ("YYYY-Www-D", "1601-W01-1") <> 0
                DISPLAY "Test 6 failed" END-DISPLAY
            END-IF


            *> How will this work with zero-length items?
            IF FUNCTION TEST-FORMATTED-DATETIME
                   ("YYYYMMDD", "1") <> 2
                DISPLAY "Test 7 failed" END-DISPLAY
            END-IF
            IF FUNCTION TEST-FORMATTED-DATETIME
                   ("YYYYMMDD", "160A0101") <> 4
                DISPLAY "Test 8 failed" END-DISPLAY
            END-IF
            IF FUNCTION TEST-FORMATTED-DATETIME
                   ("YYYYMMDD", "00000101") <> 1
                DISPLAY "Test 9 failed" END-DISPLAY
            END-IF
            IF FUNCTION TEST-FORMATTED-DATETIME
                   ("YYYYMMDD", "16000101") <> 4
                DISPLAY "Test 10 failed" END-DISPLAY
            END-IF
            IF FUNCTION TEST-FORMATTED-DATETIME
                   ("YYYYMMDD", "16010001") <> 6
                DISPLAY "Test 11 failed" END-DISPLAY
            END-IF
            IF FUNCTION TEST-FORMATTED-DATETIME
                   ("YYYYMMDD", "16011301") <> 6
                DISPLAY "Test 12 failed" END-DISPLAY
            END-IF
            IF FUNCTION TEST-FORMATTED-DATETIME
                   ("YYYYMMDD", "16010190") <> 7
                DISPLAY "Test 13 failed" END-DISPLAY
            END-IF
            IF FUNCTION TEST-FORMATTED-DATETIME
                   ("YYYYMMDD", "18000229") <> 8
                DISPLAY "Test 14 failed" END-DISPLAY
            END-IF
            IF FUNCTION TEST-FORMATTED-DATETIME
                   ("YYYY-MM-DD", "1601 01 01") <> 5
                DISPLAY "Test 15 failed" END-DISPLAY
            END-IF
            IF FUNCTION TEST-FORMATTED-DATETIME
                   ("YYYYMMDD", "160101010") <> 9
                DISPLAY "Test 16 failed" END-DISPLAY
            END-IF
            IF FUNCTION TEST-FORMATTED-DATETIME
                   ("YYYYWwwD", "1601A011") <> 5
                DISPLAY "Test 17 failed" END-DISPLAY
            END-IF
            IF FUNCTION TEST-FORMATTED-DATETIME
                   ("YYYYWwwD", "1601W531") <> 7
                DISPLAY "Test 18 failed" END-DISPLAY
            END-IF
            IF FUNCTION TEST-FORMATTED-DATETIME
                   ("YYYYWwwD", "1601W601") <> 6
                DISPLAY "Test 19 failed" END-DISPLAY
            END-IF
            IF FUNCTION TEST-FORMATTED-DATETIME
                   ("YYYYWwwD", "2009W531") <> 0
                DISPLAY "Test 20 failed" END-DISPLAY
            END-IF
            IF FUNCTION TEST-FORMATTED-DATETIME
                   ("YYYYWwwD", "1601W018") <> 8
                DISPLAY "Test 21 failed" END-DISPLAY
            END-IF
            IF FUNCTION TEST-FORMATTED-DATETIME
                   ("YYYYDDD", "1601366") <> 7
                DISPLAY "Test 22 failed" END-DISPLAY
            END-IF
            IF FUNCTION TEST-FORMATTED-DATETIME
                   ("YYYYDDD", "1601370") <> 6
                DISPLAY "Test 23 failed" END-DISPLAY
            END-IF
            IF FUNCTION TEST-FORMATTED-DATETIME
                   ("YYYYDDD", "1601400") <> 5
                DISPLAY "Test 24 failed" END-DISPLAY
            END-IF
            IF FUNCTION TEST-FORMATTED-DATETIME
                   ("YYYYMMDD", "01") <> 1
                DISPLAY "Test 25 failed" END-DISPLAY
            END-IF
            IF FUNCTION TEST-FORMATTED-DATETIME
                   ("YYYYMMDD", "1601010") <> 8
                DISPLAY "Test 26 failed" END-DISPLAY
            END-IF

            STOP RUN
            .

