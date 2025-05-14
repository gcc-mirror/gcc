       *> { dg-do run }
       *> { dg-set-target-env-var GCOBOL_CURRENT_DATE "2015/04/05 18:45:22" }
       *> { dg-output-file "group2/ACCEPT_FROM_TIME___DATE___DAY___DAY-OF-WEEK__2_.out" }

       IDENTIFICATION DIVISION.
       PROGRAM-ID. prog.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       *> one byte longer to make sure there is no garbage in
       01  WS-YYYYMMDD   PIC X(9).
       01  WS-YYYYDDD    PIC X(8).
       01  WS-DAYOFWEEK  PIC X(2).
       01  WS-DATE-TODAY.
           05  WS-TODAYS-YY            PIC 9(02) VALUE 0.
           05  WS-TODAYS-MM            PIC 9(02) VALUE 0.
           05  WS-TODAYS-DD            PIC 9(02) VALUE 0.

       01  WS-DATE.
           05  WS-DATE-MM              PIC 9(02) VALUE 0.
           05  FILLER                  PIC X(01) VALUE '/'.
           05  WS-DATE-DD              PIC 9(02) VALUE 0.
           05  FILLER                  PIC X(01) VALUE '/'.
           05  WS-DATE-YY              PIC 9(02) VALUE 0.

       01  WS-TIME-NOW.
           05  WS-NOW-HH               PIC 9(02) VALUE 0.
           05  WS-NOW-MM               PIC 9(02) VALUE 0.
           05  WS-NOW-SS               PIC 9(02) VALUE 0.
           05  WS-NOW-HS               PIC 9(02) VALUE 0.

       01  WS-TIME.
           05  WS-TIME-HH              PIC 9(02) VALUE 0.
           05  FILLER                  PIC X(01) VALUE ':'.
           05  WS-TIME-MM              PIC 9(02) VALUE 0.
           05  FILLER                  PIC X(01) VALUE ':'.
           05  WS-TIME-SS              PIC 9(02) VALUE 0.

       PROCEDURE DIVISION.
           ACCEPT WS-DATE-TODAY FROM DATE
           ACCEPT WS-TIME-NOW   FROM TIME
           MOVE WS-TODAYS-YY TO WS-DATE-YY
           MOVE WS-TODAYS-MM TO WS-DATE-MM
           MOVE WS-TODAYS-DD TO WS-DATE-DD
           MOVE WS-NOW-HH    TO WS-TIME-HH
           MOVE WS-NOW-MM    TO WS-TIME-MM
           MOVE WS-NOW-SS    TO WS-TIME-SS
           DISPLAY 'PROCESS DATE/TIME : ' WS-DATE SPACE WS-TIME
           END-DISPLAY
           ACCEPT  WS-YYYYMMDD   FROM DATE YYYYMMDD
           DISPLAY WS-YYYYMMDD(1:8)
           IF WS-YYYYMMDD not = "20150405"
              DISPLAY 'Wrong date DATE YYYYMMDD: ' WS-YYYYMMDD
                      ' expected: 20150405'
                      UPON STDERR
              END-DISPLAY
           END-IF
           ACCEPT  WS-YYYYDDD    FROM DAY  YYYYDDD
           DISPLAY WS-YYYYDDD(1:7)
           IF WS-YYYYDDD  not = "2015095"
              DISPLAY 'Wrong date YYYYDDD:  '      WS-YYYYDDD
                      ' expected: 2015095'
                      UPON STDERR
              END-DISPLAY
           END-IF
           ACCEPT  WS-DAYOFWEEK    FROM DAY-OF-WEEK
           DISPLAY WS-DAYOFWEEK(1:1)
           IF WS-DAYOFWEEK  not = "7"
              DISPLAY 'Wrong date DAYOFWEEK: '     WS-DAYOFWEEK
                      ' expected: 7'
                      UPON STDERR
              END-DISPLAY
           END-IF
           STOP RUN.

