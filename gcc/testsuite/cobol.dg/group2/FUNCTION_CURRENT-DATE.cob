       *> { dg-do run }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       ENVIRONMENT      DIVISION.
       DATA             DIVISION.
       WORKING-STORAGE SECTION.
       01  TEST-FLD.
           02  WS-YEAR            PIC 9(04).
               88 VALID-YEAR      VALUE 1980 THRU 9999.
           02  WS-MONTH           PIC 9(02).
               88 VALID-MONTH     VALUE 01 THRU 12.
           02  WS-DAY             PIC 9(02).
               88 VALID-DAY       VALUE 01 THRU 31.
           02  WS-HOUR            PIC 9(02).
               88 VALID-HOUR      VALUE 00 THRU 23.
           02  WS-MIN             PIC 9(02).
               88 VALID-MIN       VALUE 00 THRU 59.
           02  WS-SEVALIDD        PIC 9(02).
               88 VALID-SEC       VALUE 00 THRU 59.
           02  WS-HUNDSEC         PIC 9(02).
               88 VALID-HUNDSEC   VALUE 00 THRU 99.
           02  WS-GREENW          PIC X.
               88 VALID-GREENW    VALUE "-", "+", "0".
               88 ZERO-GREENW     VALUE "0".
           02  WS-OFFSET          PIC 9(02).
               88 VALID-OFFSET    VALUE 00 THRU 13.
               88 ZERO-OFFSET     VALUE 00.
           02  WS-OFFSET2         PIC 9(02).
               88 VALID-OFFSET2   VALUE 00 THRU 59.
               88 ZERO-OFFSET2    VALUE 00.
           02  WS-UNSET           PIC X VALUE '_'.
               88 VALID-UNSET     VALUE '_'.
       PROCEDURE        DIVISION.
           STRING FUNCTION CURRENT-DATE
                  DELIMITED BY SIZE
                  INTO TEST-FLD
           END-STRING.
           EVALUATE TRUE
              WHEN NOT VALID-UNSET
                 DISPLAY "FUNCTION result too long"
                 END-DISPLAY
              WHEN VALID-YEAR     AND
                 VALID-MONTH    AND
                 VALID-DAY      AND
                 VALID-HOUR     AND
                 VALID-MIN      AND
                 VALID-SEC      AND
                 VALID-HUNDSEC  AND
                 VALID-GREENW   AND
                 VALID-OFFSET   AND
                 VALID-OFFSET2  AND
                 VALID-UNSET    AND
                 ((NOT ZERO-GREENW) OR (ZERO-OFFSET AND ZERO-OFFSET2))
                 CONTINUE
              WHEN OTHER
                 DISPLAY "CURRENT-DATE with wrong format: "
                         TEST-FLD (01:21)
                 END-DISPLAY
           END-EVALUATE.
           STOP RUN.

