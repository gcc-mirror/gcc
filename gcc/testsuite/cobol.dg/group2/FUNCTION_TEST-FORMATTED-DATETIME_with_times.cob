       *> { dg-do run }

        IDENTIFICATION   DIVISION.
        PROGRAM-ID.      prog.
        DATA             DIVISION.
        WORKING-STORAGE  SECTION.
        PROCEDURE        DIVISION.
            IF FUNCTION TEST-FORMATTED-DATETIME
                    ("hhmmss.sssssssssZ", "000000.000000000Z") <> 0
                DISPLAY "Test 1 failed" END-DISPLAY
            END-IF
            IF FUNCTION TEST-FORMATTED-DATETIME
                    ("hh:mm:ss.sssssssssZ", "00:00:00.000000000Z") <> 0
                DISPLAY "Test 2 failed" END-DISPLAY
            END-IF
            *> 0 instead of +/- valid in sending fields with offset of zero.
            IF FUNCTION TEST-FORMATTED-DATETIME
                    ("hhmmss.sssssssss+hhmm", "000000.00000000000000")
                    <> 0
                DISPLAY "Test 3 failed" END-DISPLAY
            END-IF
            IF FUNCTION TEST-FORMATTED-DATETIME
                    ("hh:mm:ss.sssssssss+hh:mm",
                    "00:00:00.000000000+00:00")
                    <> 0
                DISPLAY "Test 4 failed" END-DISPLAY
            END-IF

            IF FUNCTION TEST-FORMATTED-DATETIME
                    ("hhmmss", "300000") <> 1
                DISPLAY "Test 5 failed" END-DISPLAY
            END-IF
            IF FUNCTION TEST-FORMATTED-DATETIME
                    ("hhmmss", "250000") <> 2
                DISPLAY "Test 6 failed" END-DISPLAY
            END-IF
            IF FUNCTION TEST-FORMATTED-DATETIME
                    ("hhmmss", "006000") <> 3
                DISPLAY "Test 7 failed" END-DISPLAY
            END-IF
            IF FUNCTION TEST-FORMATTED-DATETIME
                    ("hhmmss", "000060") <> 5
                DISPLAY "Test 8 failed" END-DISPLAY
            END-IF
            IF FUNCTION TEST-FORMATTED-DATETIME
                    ("hh:mm:ss", "00-00-00") <> 3
                DISPLAY "Test 9 failed" END-DISPLAY
            END-IF
            IF FUNCTION TEST-FORMATTED-DATETIME
                    ("hhmmss.ss", "000000,00") <> 7
                DISPLAY "Test 10 failed" END-DISPLAY
            END-IF
            IF FUNCTION TEST-FORMATTED-DATETIME
                    ("hhmmss+hhmm", "000000 0000") <> 7
                DISPLAY "Test 11 failed" END-DISPLAY
            END-IF
            IF FUNCTION TEST-FORMATTED-DATETIME
                    ("hhmmss+hhmm", "00000000001") <> 11
                DISPLAY "Test 12 failed" END-DISPLAY
            END-IF
            IF FUNCTION TEST-FORMATTED-DATETIME
                    ("hhmmssZ", "000000A") <> 7
                DISPLAY "Test 13 failed" END-DISPLAY
            END-IF
            IF FUNCTION TEST-FORMATTED-DATETIME
                    ("hhmmss", SPACE) <> 1
                DISPLAY "Test 14 failed" END-DISPLAY
            END-IF

            STOP RUN
            .

