       *> { dg-do run }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01  result       PIC 9(8)V9(9) COMP-5.
       PROCEDURE        DIVISION.
           MOVE FUNCTION SECONDS-FROM-FORMATTED-TIME
                    ("hhmmss", "010203")
               TO result.
           IF result NOT = 3723
                   DISPLAY "Test 1 failed: " result
                   END-DISPLAY
           END-IF.

           MOVE FUNCTION SECONDS-FROM-FORMATTED-TIME
                    ("hh:mm:ss", "01:02:03")
               TO result.
           IF result NOT = 3723
                   DISPLAY "Test 2 failed: " result
                   END-DISPLAY
           END-IF.

           MOVE FUNCTION SECONDS-FROM-FORMATTED-TIME
                    ("hhmmss.ssssssss", "010203.04050607")
               TO result.
           IF result NOT = 3723.04050607
                   DISPLAY "Test 3 failed: " result
                   END-DISPLAY
           END-IF.

           MOVE FUNCTION SECONDS-FROM-FORMATTED-TIME
                    ("hhmmssZ", "010203Z")
               TO result.
           IF result NOT = 3723
                   DISPLAY "Test 4 failed: " result
                   END-DISPLAY
           END-IF.

           MOVE FUNCTION SECONDS-FROM-FORMATTED-TIME
                    ("hhmmss+hhmm", "010203+0405")
               TO result.
           IF result NOT = 3723
                   DISPLAY "Test 5 failed: " result
                   END-DISPLAY
           END-IF.

           MOVE FUNCTION SECONDS-FROM-FORMATTED-TIME
                    ("YYYYMMDDThhmmss", "16010101T010203")
               TO result.
           IF result NOT = 3723
                   DISPLAY "Test 6 failed: " result
                   END-DISPLAY
           END-IF.

           STOP RUN.

