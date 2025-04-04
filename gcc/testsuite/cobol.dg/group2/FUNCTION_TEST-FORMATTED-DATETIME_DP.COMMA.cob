       *> { dg-do run }

        IDENTIFICATION   DIVISION.
        PROGRAM-ID.      prog.
        ENVIRONMENT      DIVISION.
        CONFIGURATION    SECTION.
        SPECIAL-NAMES.
            DECIMAL-POINT IS COMMA.
        DATA             DIVISION.
        WORKING-STORAGE  SECTION.
        PROCEDURE        DIVISION.
            IF FUNCTION TEST-FORMATTED-DATETIME
                    ("hhmmss,ss", "000000,00") <> 0
                DISPLAY "Test 1 failed" END-DISPLAY
            END-IF
            IF FUNCTION TEST-FORMATTED-DATETIME
                    ("YYYYMMDDThhmmss,ss", "16010101T000000,00") <> 0
                DISPLAY "Test 2 failed" END-DISPLAY
            END-IF

            IF FUNCTION TEST-FORMATTED-DATETIME
                    ("hhmmss,ss", "000000.00") <> 7
                DISPLAY "Test 3 failed" END-DISPLAY
            END-IF
            IF FUNCTION TEST-FORMATTED-DATETIME
                    ("YYYYMMDDThhmmss,ss", "16010101T000000.00") <> 16
                DISPLAY "Test 4 failed" END-DISPLAY
            END-IF

            STOP RUN
            .

