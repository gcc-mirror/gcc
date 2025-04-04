       *> { dg-do run }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01  day-int      PIC 9(9).

       PROCEDURE        DIVISION.
           *> The date 2013-12-30 is used as it can also be used to
           *> check the conversion of dates in week form.
           MOVE FUNCTION INTEGER-OF-FORMATTED-DATE
                   ("YYYY-MM-DD", "2013-12-30")
               TO day-int
           IF day-int <> 150844
               DISPLAY "Test 1 failed: " day-int END-DISPLAY
           END-IF

           MOVE FUNCTION INTEGER-OF-FORMATTED-DATE
                   ("YYYY-DDD", "2013-364")
               TO day-int
           IF day-int <> 150844
               DISPLAY "Test 2 failed: " day-int END-DISPLAY
           END-IF

           MOVE FUNCTION INTEGER-OF-FORMATTED-DATE
                   ("YYYY-Www-D", "2014-W01-1")
               TO day-int
           IF day-int <> 150844
               DISPLAY "Test 3 failed: " day-int END-DISPLAY
           END-IF

           MOVE FUNCTION INTEGER-OF-FORMATTED-DATE
                   ("YYYY-MM-DDThh:mm:ss", "2013-12-30T12:34:56")
               TO day-int
           IF day-int <> 150844
               DISPLAY "Test 4 failed: " day-int END-DISPLAY
           END-IF

           STOP RUN.

