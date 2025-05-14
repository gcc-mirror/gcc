       *> { dg-do run }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01  str          PIC X(40).
       PROCEDURE        DIVISION.
      *>   Test normal inputs.
           MOVE FUNCTION FORMATTED-DATETIME
                   ("YYYYMMDDThhmmss", 1, 45296)
               TO str
           IF str <> "16010101T123456"
               DISPLAY "Test 1 failed: " str END-DISPLAY
           END-IF

           MOVE FUNCTION FORMATTED-DATETIME
                   ("YYYY-MM-DDThh:mm:ss", 1, 45296)
               TO str
           IF str <> "1601-01-01T12:34:56"
               DISPLAY "Test 2 failed: " str END-DISPLAY
           END-IF

           MOVE FUNCTION FORMATTED-DATETIME
                    ("YYYYDDDThhmmss+hhmm", 1, 45296, -754)
               TO str
           IF str <> "1601001T123456-1234"
               DISPLAY "Test 3 failed: " str END-DISPLAY
           END-IF

           MOVE FUNCTION FORMATTED-DATETIME
                    ("YYYYDDDThhmmss+hhmm", 1, 45296)
               TO str
           IF str <> "1601001T123456+0000"
               DISPLAY "Test 4 failed: " str END-DISPLAY
           END-IF

           *> Test underflow to next day due to offset
           MOVE FUNCTION FORMATTED-DATETIME
                    ("YYYYDDDThhmmss.sssssssssZ", 150846, 0,
                     1)
               TO str
           IF str <> "2013365T235900.000000000Z"
               DISPLAY "Test 5 failed: " str END-DISPLAY
           END-IF

           STOP RUN.

