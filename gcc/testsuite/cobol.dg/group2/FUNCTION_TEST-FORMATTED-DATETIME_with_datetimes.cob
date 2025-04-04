       *> { dg-do run }

        IDENTIFICATION   DIVISION.
        PROGRAM-ID.      prog.
        DATA             DIVISION.
        WORKING-STORAGE  SECTION.
        77 RESULT        PIC 9(02).
        PROCEDURE        DIVISION.
            MOVE FUNCTION TEST-FORMATTED-DATETIME
                    ("YYYYMMDDThhmmss", "16010101T000000")
              TO RESULT
            IF RESULT <> 0
               DISPLAY "Test 1 failed: " RESULT END-DISPLAY
            END-IF
            MOVE FUNCTION TEST-FORMATTED-DATETIME
                    ("YYYY-MM-DDThh:mm:ss.sssssssss+hh:mm",
                    "1601-01-01T00:00:00.000000000+00:00")
              TO RESULT
            IF RESULT <> 0
               DISPLAY "Test 2 failed: " RESULT END-DISPLAY
            END-IF

            MOVE FUNCTION TEST-FORMATTED-DATETIME
                    ("YYYYMMDDThhmmss", "16010101 000000")
              TO RESULT
            IF RESULT <> 9
               DISPLAY "Test 3 failed: " RESULT END-DISPLAY
            END-IF
            MOVE FUNCTION TEST-FORMATTED-DATETIME
                    ("YYYYMMDDThhmmss", SPACE)
              TO RESULT
            IF RESULT <> 1
               DISPLAY "Test 4 failed: " RESULT END-DISPLAY
            END-IF
            MOVE FUNCTION TEST-FORMATTED-DATETIME
                    ("YYYYMMDDThhmmss", "16010101T      ")
              TO RESULT
            IF RESULT <> 10
               DISPLAY "Test 5 failed: " RESULT END-DISPLAY
            END-IF

            STOP RUN
            .

