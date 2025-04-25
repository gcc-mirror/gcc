       *> { dg-do run }
       *> { dg-output-file "group2/ROUNDING_omnibus_NumericDisplay_from_COMPUTE.out" }
        IDENTIFICATION DIVISION.
        PROGRAM-ID. prog.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01 VAR1             PICTURE        S999V9.
        01 VAR2             PICTURE        S999.
        01 SHOULD_BE        PICTURE        S999.
        01 RMODE            PICTURE        X(64).
        01 EMPTY            PIC X VALUE " ".
        01 FLAG             PIC X.
        PROCEDURE DIVISION.

        DISPLAY "ROUNDING from NumericDisplay after COMPUTE."

        PERFORM truncation-e.
        PERFORM truncation-m.
        PERFORM nearest-away-from-zero-e.
        PERFORM nearest-away-from-zero-m.
        PERFORM away-from-zero-e.
        PERFORM away-from-zero-m.
        PERFORM nearest-even-e.
        PERFORM nearest-even-m.
        PERFORM nearest-toward-zero-e.
        PERFORM nearest-toward-zero-m.
        PERFORM toward-greater-e.
        PERFORM toward-greater-m.
        PERFORM toward-lesser-e.
        PERFORM toward-lesser-m.
        PERFORM prohibited-e.
        GOBACK.

        truncation-e.
            MOVE "TRUNCATION" TO RMODE
            MOVE 111.0        TO VAR1
            MOVE 111          TO SHOULD_BE
            COMPUTE VAR2 ROUNDED MODE TRUNCATION = VAR1
            PERFORM SHOW_RESULTS.
            MOVE "TRUNCATION" TO RMODE
            MOVE 111.1        TO VAR1
            MOVE 111          TO SHOULD_BE
            COMPUTE VAR2 ROUNDED MODE TRUNCATION = VAR1
            PERFORM SHOW_RESULTS.
            MOVE "TRUNCATION" TO RMODE
            MOVE 111.5        TO VAR1
            MOVE 111          TO SHOULD_BE
            COMPUTE VAR2 ROUNDED MODE TRUNCATION = VAR1
            PERFORM SHOW_RESULTS.
            MOVE "TRUNCATION" TO RMODE
            MOVE 111.9        TO VAR1
            MOVE 111          TO SHOULD_BE
            COMPUTE VAR2 ROUNDED MODE TRUNCATION = VAR1
            PERFORM SHOW_RESULTS.

        truncation-m.
            MOVE "TRUNCATION" TO RMODE
            MOVE -111.0        TO VAR1
            MOVE -111          TO SHOULD_BE
            COMPUTE VAR2 ROUNDED MODE TRUNCATION = VAR1
            PERFORM SHOW_RESULTS.
            MOVE "TRUNCATION" TO RMODE
            MOVE -111.1        TO VAR1
            MOVE -111          TO SHOULD_BE
            COMPUTE VAR2 ROUNDED MODE TRUNCATION = VAR1
            PERFORM SHOW_RESULTS.
            MOVE "TRUNCATION" TO RMODE
            MOVE -111.5        TO VAR1
            MOVE -111          TO SHOULD_BE
            COMPUTE VAR2 ROUNDED MODE TRUNCATION = VAR1
            PERFORM SHOW_RESULTS.
            MOVE "TRUNCATION" TO RMODE
            MOVE -111.9        TO VAR1
            MOVE -111          TO SHOULD_BE
            COMPUTE VAR2 ROUNDED MODE TRUNCATION = VAR1
            PERFORM SHOW_RESULTS.

        nearest-away-from-zero-e.
            MOVE "NEAREST-AWAY-FROM-ZERO" TO RMODE
            MOVE 111.0        TO VAR1
            MOVE 111          TO SHOULD_BE
            COMPUTE VAR2 ROUNDED MODE NEAREST-AWAY-FROM-ZERO = VAR1
            PERFORM SHOW_RESULTS.
            MOVE "NEAREST-AWAY-FROM-ZERO" TO RMODE
            MOVE 111.1        TO VAR1
            MOVE 111          TO SHOULD_BE
            COMPUTE VAR2 ROUNDED MODE NEAREST-AWAY-FROM-ZERO = VAR1
            PERFORM SHOW_RESULTS.
            MOVE "NEAREST-AWAY-FROM-ZERO" TO RMODE
            MOVE 111.5        TO VAR1
            MOVE 112          TO SHOULD_BE
            COMPUTE VAR2 ROUNDED MODE NEAREST-AWAY-FROM-ZERO = VAR1
            PERFORM SHOW_RESULTS.
            MOVE "NEAREST-AWAY-FROM-ZERO" TO RMODE
            MOVE 111.9        TO VAR1
            MOVE 112          TO SHOULD_BE
            COMPUTE VAR2 ROUNDED MODE NEAREST-AWAY-FROM-ZERO = VAR1
            PERFORM SHOW_RESULTS.

        nearest-away-from-zero-m.
            MOVE "NEAREST-AWAY-FROM-ZERO" TO RMODE
            MOVE -111.0        TO VAR1
            MOVE -111          TO SHOULD_BE
            COMPUTE VAR2 ROUNDED MODE NEAREST-AWAY-FROM-ZERO = VAR1
            PERFORM SHOW_RESULTS.
            MOVE "NEAREST-AWAY-FROM-ZERO" TO RMODE
            MOVE -111.1        TO VAR1
            MOVE -111          TO SHOULD_BE
            COMPUTE VAR2 ROUNDED MODE NEAREST-AWAY-FROM-ZERO = VAR1
            PERFORM SHOW_RESULTS.
            MOVE "NEAREST-AWAY-FROM-ZERO" TO RMODE
            MOVE -111.5        TO VAR1
            MOVE -112          TO SHOULD_BE
            COMPUTE VAR2 ROUNDED MODE NEAREST-AWAY-FROM-ZERO = VAR1
            PERFORM SHOW_RESULTS.
            MOVE "NEAREST-AWAY-FROM-ZERO" TO RMODE
            MOVE -111.9        TO VAR1
            MOVE -112          TO SHOULD_BE
            COMPUTE VAR2 ROUNDED MODE NEAREST-AWAY-FROM-ZERO = VAR1
            PERFORM SHOW_RESULTS.

        away-from-zero-e.
            MOVE "AWAY-FROM-ZERO" TO RMODE
            MOVE 111.0        TO VAR1
            MOVE 111          TO SHOULD_BE
            COMPUTE VAR2 ROUNDED MODE AWAY-FROM-ZERO = VAR1
            PERFORM SHOW_RESULTS.
            MOVE "AWAY-FROM-ZERO" TO RMODE
            MOVE 111.1        TO VAR1
            MOVE 112          TO SHOULD_BE
            COMPUTE VAR2 ROUNDED MODE AWAY-FROM-ZERO = VAR1
            PERFORM SHOW_RESULTS.
            MOVE "AWAY-FROM-ZERO" TO RMODE
            MOVE 111.5        TO VAR1
            MOVE 112          TO SHOULD_BE
            COMPUTE VAR2 ROUNDED MODE AWAY-FROM-ZERO = VAR1
            PERFORM SHOW_RESULTS.
            MOVE "AWAY-FROM-ZERO" TO RMODE
            MOVE 111.9        TO VAR1
            MOVE 112          TO SHOULD_BE
            COMPUTE VAR2 ROUNDED MODE AWAY-FROM-ZERO = VAR1
            PERFORM SHOW_RESULTS.

        away-from-zero-m.
            MOVE "AWAY-FROM-ZERO" TO RMODE
            MOVE -111.0        TO VAR1
            MOVE -111          TO SHOULD_BE
            COMPUTE VAR2 ROUNDED MODE AWAY-FROM-ZERO = VAR1
            PERFORM SHOW_RESULTS.
            MOVE "AWAY-FROM-ZERO" TO RMODE
            MOVE -111.1        TO VAR1
            MOVE -112          TO SHOULD_BE
            COMPUTE VAR2 ROUNDED MODE AWAY-FROM-ZERO = VAR1
            PERFORM SHOW_RESULTS.
            MOVE "AWAY-FROM-ZERO" TO RMODE
            MOVE -111.5        TO VAR1
            MOVE -112          TO SHOULD_BE
            COMPUTE VAR2 ROUNDED MODE AWAY-FROM-ZERO = VAR1
            PERFORM SHOW_RESULTS.
            MOVE "AWAY-FROM-ZERO" TO RMODE
            MOVE -111.9        TO VAR1
            MOVE -112          TO SHOULD_BE
            COMPUTE VAR2 ROUNDED MODE AWAY-FROM-ZERO = VAR1
            PERFORM SHOW_RESULTS.

        nearest-even-e.
            MOVE "NEAREST-EVEN" TO RMODE
            MOVE 110.0        TO VAR1
            MOVE 110          TO SHOULD_BE
            COMPUTE VAR2 ROUNDED MODE NEAREST-EVEN = VAR1
            PERFORM SHOW_RESULTS.
            MOVE "NEAREST-EVEN" TO RMODE
            MOVE 110.1        TO VAR1
            MOVE 110          TO SHOULD_BE
            COMPUTE VAR2 ROUNDED MODE NEAREST-EVEN = VAR1
            PERFORM SHOW_RESULTS.
            MOVE "NEAREST-EVEN" TO RMODE
            MOVE 110.5        TO VAR1
            MOVE 110          TO SHOULD_BE
            COMPUTE VAR2 ROUNDED MODE NEAREST-EVEN = VAR1
            PERFORM SHOW_RESULTS.
            MOVE "NEAREST-EVEN" TO RMODE
            MOVE 110.9        TO VAR1
            MOVE 111          TO SHOULD_BE
            COMPUTE VAR2 ROUNDED MODE NEAREST-EVEN = VAR1
            MOVE "NEAREST-EVEN" TO RMODE
            MOVE 111.0        TO VAR1
            MOVE 111          TO SHOULD_BE
            COMPUTE VAR2 ROUNDED MODE NEAREST-EVEN = VAR1
            PERFORM SHOW_RESULTS.
            MOVE "NEAREST-EVEN" TO RMODE
            MOVE 111.1        TO VAR1
            MOVE 111          TO SHOULD_BE
            COMPUTE VAR2 ROUNDED MODE NEAREST-EVEN = VAR1
            PERFORM SHOW_RESULTS.
            MOVE "NEAREST-EVEN" TO RMODE
            MOVE 111.5        TO VAR1
            MOVE 112          TO SHOULD_BE
            COMPUTE VAR2 ROUNDED MODE NEAREST-EVEN = VAR1
            PERFORM SHOW_RESULTS.
            MOVE "NEAREST-EVEN" TO RMODE
            MOVE 111.9        TO VAR1
            MOVE 112          TO SHOULD_BE
            COMPUTE VAR2 ROUNDED MODE NEAREST-EVEN = VAR1
            PERFORM SHOW_RESULTS.

        nearest-even-m.
            MOVE "NEAREST-EVEN" TO RMODE
            MOVE -110.0        TO VAR1
            MOVE -110          TO SHOULD_BE
            COMPUTE VAR2 ROUNDED MODE NEAREST-EVEN = VAR1
            PERFORM SHOW_RESULTS.
            MOVE "NEAREST-EVEN" TO RMODE
            MOVE -110.1        TO VAR1
            MOVE -110          TO SHOULD_BE
            COMPUTE VAR2 ROUNDED MODE NEAREST-EVEN = VAR1
            PERFORM SHOW_RESULTS.
            MOVE "NEAREST-EVEN" TO RMODE
            MOVE -110.5        TO VAR1
            MOVE -110          TO SHOULD_BE
            COMPUTE VAR2 ROUNDED MODE NEAREST-EVEN = VAR1
            PERFORM SHOW_RESULTS.
            MOVE "NEAREST-EVEN" TO RMODE
            MOVE -110.9        TO VAR1
            MOVE -111          TO SHOULD_BE
            COMPUTE VAR2 ROUNDED MODE NEAREST-EVEN = VAR1
            MOVE "NEAREST-EVEN" TO RMODE
            MOVE -111.0        TO VAR1
            MOVE -111          TO SHOULD_BE
            COMPUTE VAR2 ROUNDED MODE NEAREST-EVEN = VAR1
            PERFORM SHOW_RESULTS.
            MOVE "NEAREST-EVEN" TO RMODE
            MOVE -111.1        TO VAR1
            MOVE -111          TO SHOULD_BE
            COMPUTE VAR2 ROUNDED MODE NEAREST-EVEN = VAR1
            PERFORM SHOW_RESULTS.
            MOVE "NEAREST-EVEN" TO RMODE
            MOVE -111.5        TO VAR1
            MOVE -112          TO SHOULD_BE
            COMPUTE VAR2 ROUNDED MODE NEAREST-EVEN = VAR1
            PERFORM SHOW_RESULTS.
            MOVE "NEAREST-EVEN" TO RMODE
            MOVE -111.9        TO VAR1
            MOVE -112          TO SHOULD_BE
            COMPUTE VAR2 ROUNDED MODE NEAREST-EVEN = VAR1
            PERFORM SHOW_RESULTS.

        nearest-toward-zero-e.
            MOVE "NEAREST-TOWARD-ZERO" TO RMODE
            MOVE 111.0        TO VAR1
            MOVE 111          TO SHOULD_BE
            COMPUTE VAR2 ROUNDED MODE NEAREST-TOWARD-ZERO = VAR1
            PERFORM SHOW_RESULTS.
            MOVE "NEAREST-TOWARD-ZERO" TO RMODE
            MOVE 111.1        TO VAR1
            MOVE 111          TO SHOULD_BE
            COMPUTE VAR2 ROUNDED MODE NEAREST-TOWARD-ZERO = VAR1
            PERFORM SHOW_RESULTS.
            MOVE "NEAREST-TOWARD-ZERO" TO RMODE
            MOVE 111.5        TO VAR1
            MOVE 111          TO SHOULD_BE
            COMPUTE VAR2 ROUNDED MODE NEAREST-TOWARD-ZERO = VAR1
            PERFORM SHOW_RESULTS.
            MOVE "NEAREST-TOWARD-ZERO" TO RMODE
            MOVE 111.9        TO VAR1
            MOVE 112          TO SHOULD_BE
            COMPUTE VAR2 ROUNDED MODE NEAREST-TOWARD-ZERO = VAR1
            PERFORM SHOW_RESULTS.

        nearest-toward-zero-m.
            MOVE "NEAREST-TOWARD-ZERO" TO RMODE
            MOVE -111.0        TO VAR1
            MOVE -111          TO SHOULD_BE
            COMPUTE VAR2 ROUNDED MODE NEAREST-TOWARD-ZERO = VAR1
            PERFORM SHOW_RESULTS.
            MOVE "NEAREST-TOWARD-ZERO" TO RMODE
            MOVE -111.1        TO VAR1
            MOVE -111          TO SHOULD_BE
            COMPUTE VAR2 ROUNDED MODE NEAREST-TOWARD-ZERO = VAR1
            PERFORM SHOW_RESULTS.
            MOVE "NEAREST-TOWARD-ZERO" TO RMODE
            MOVE -111.5        TO VAR1
            MOVE -111          TO SHOULD_BE
            COMPUTE VAR2 ROUNDED MODE NEAREST-TOWARD-ZERO = VAR1
            PERFORM SHOW_RESULTS.
            MOVE "NEAREST-TOWARD-ZERO" TO RMODE
            MOVE -111.9        TO VAR1
            MOVE -112          TO SHOULD_BE
            COMPUTE VAR2 ROUNDED MODE NEAREST-TOWARD-ZERO = VAR1
            PERFORM SHOW_RESULTS.

        toward-greater-e.
            MOVE "TOWARD-GREATER" TO RMODE
            MOVE 111.0        TO VAR1
            MOVE 111          TO SHOULD_BE
            COMPUTE VAR2 ROUNDED MODE TOWARD-GREATER = VAR1
            PERFORM SHOW_RESULTS.
            MOVE "TOWARD-GREATER" TO RMODE
            MOVE 111.1        TO VAR1
            MOVE 112          TO SHOULD_BE
            COMPUTE VAR2 ROUNDED MODE TOWARD-GREATER = VAR1
            PERFORM SHOW_RESULTS.
            MOVE "TOWARD-GREATER" TO RMODE
            MOVE 111.5        TO VAR1
            MOVE 112          TO SHOULD_BE
            COMPUTE VAR2 ROUNDED MODE TOWARD-GREATER = VAR1
            PERFORM SHOW_RESULTS.
            MOVE "TOWARD-GREATER" TO RMODE
            MOVE 111.9        TO VAR1
            MOVE 112          TO SHOULD_BE
            COMPUTE VAR2 ROUNDED MODE TOWARD-GREATER = VAR1
            PERFORM SHOW_RESULTS.

        toward-greater-m.
            MOVE "TOWARD-GREATER" TO RMODE
            MOVE -111.0        TO VAR1
            MOVE -111          TO SHOULD_BE
            COMPUTE VAR2 ROUNDED MODE TOWARD-GREATER = VAR1
            PERFORM SHOW_RESULTS.
            MOVE "TOWARD-GREATER" TO RMODE
            MOVE -111.1        TO VAR1
            MOVE -111          TO SHOULD_BE
            COMPUTE VAR2 ROUNDED MODE TOWARD-GREATER = VAR1
            PERFORM SHOW_RESULTS.
            MOVE "TOWARD-GREATER" TO RMODE
            MOVE -111.5        TO VAR1
            MOVE -111          TO SHOULD_BE
            COMPUTE VAR2 ROUNDED MODE TOWARD-GREATER = VAR1
            PERFORM SHOW_RESULTS.
            MOVE "TOWARD-GREATER" TO RMODE
            MOVE -111.9        TO VAR1
            MOVE -111          TO SHOULD_BE
            COMPUTE VAR2 ROUNDED MODE TOWARD-GREATER = VAR1
            PERFORM SHOW_RESULTS.

        toward-lesser-e.
            MOVE "TOWARD-LESSER" TO RMODE
            MOVE 111.0        TO VAR1
            MOVE 111          TO SHOULD_BE
            COMPUTE VAR2 ROUNDED MODE TOWARD-LESSER = VAR1
            PERFORM SHOW_RESULTS.
            MOVE "TOWARD-LESSER" TO RMODE
            MOVE 111.1        TO VAR1
            MOVE 111          TO SHOULD_BE
            COMPUTE VAR2 ROUNDED MODE TOWARD-LESSER = VAR1
            PERFORM SHOW_RESULTS.
            MOVE "TOWARD-LESSER" TO RMODE
            MOVE 111.5        TO VAR1
            MOVE 111          TO SHOULD_BE
            COMPUTE VAR2 ROUNDED MODE TOWARD-LESSER = VAR1
            PERFORM SHOW_RESULTS.
            MOVE "TOWARD-LESSER" TO RMODE
            MOVE 111.9        TO VAR1
            MOVE 111          TO SHOULD_BE
            COMPUTE VAR2 ROUNDED MODE TOWARD-LESSER = VAR1
            PERFORM SHOW_RESULTS.

        toward-lesser-m.
            MOVE "TOWARD-LESSER" TO RMODE
            MOVE -111.0        TO VAR1
            MOVE -111          TO SHOULD_BE
            COMPUTE VAR2 ROUNDED MODE TOWARD-LESSER = VAR1
            PERFORM SHOW_RESULTS.
            MOVE "TOWARD-LESSER" TO RMODE
            MOVE -111.1        TO VAR1
            MOVE -112          TO SHOULD_BE
            COMPUTE VAR2 ROUNDED MODE TOWARD-LESSER = VAR1
            PERFORM SHOW_RESULTS.
            MOVE "TOWARD-LESSER" TO RMODE
            MOVE -111.5        TO VAR1
            MOVE -112          TO SHOULD_BE
            COMPUTE VAR2 ROUNDED MODE TOWARD-LESSER = VAR1
            PERFORM SHOW_RESULTS.
            MOVE "TOWARD-LESSER" TO RMODE
            MOVE -111.9        TO VAR1
            MOVE -112          TO SHOULD_BE
            COMPUTE VAR2 ROUNDED MODE TOWARD-LESSER = VAR1
            PERFORM SHOW_RESULTS.

        prohibited-e.
            MOVE "PROHIBITED - fits" TO RMODE
            SET LAST EXCEPTION TO OFF
            MOVE 123          TO VAR2
            MOVE 111.0        TO VAR1
            MOVE 111          TO SHOULD_BE
            COMPUTE VAR2 ROUNDED MODE PROHIBITED = VAR1
            PERFORM SHOW_RESULTS
            DISPLAY "     EXCEPTION STATUS IS "
                        """" FUNCTION TRIM(FUNCTION EXCEPTION-STATUS) """".

            MOVE "PROHIBITED - doesn't fit; no ON ERROR phrase" TO RMODE
            SET LAST EXCEPTION TO OFF
            MOVE 123          TO VAR2
            MOVE 111.5        TO VAR1
            MOVE 123          TO SHOULD_BE
            COMPUTE VAR2 ROUNDED MODE PROHIBITED = VAR1
            PERFORM SHOW_RESULTS
            DISPLAY "     EXCEPTION STATUS IS "
                        """" FUNCTION TRIM(FUNCTION EXCEPTION-STATUS) """".

            MOVE "PROHIBITED - doesn't fit; ON ERROR phrase" TO RMODE
            SET LAST EXCEPTION TO OFF
            MOVE SPACE TO FLAG
            MOVE 123          TO VAR2
            MOVE 111.5        TO VAR1
            MOVE 123          TO SHOULD_BE
            COMPUTE VAR2 ROUNDED MODE PROHIBITED = VAR1
                ON SIZE ERROR MOVE 'X' TO FLAG
                END-COMPUTE
            PERFORM SHOW_RESULTS
            IF FLAG EQUAL 'X'
                DISPLAY "     COMPUTE had an ON SIZE error"
                END-IF.
            DISPLAY "     EXCEPTION STATUS IS "
                        """" FUNCTION TRIM(FUNCTION EXCEPTION-STATUS) """".

        SHOW_RESULTS.
            DISPLAY "Rounding mode " FUNCTION TRIM(RMODE)
                    " " VAR1 " becomes " VAR2
                    WITH NO ADVANCING
            END-DISPLAY
            IF VAR2 EQUALS SHOULD_BE
                DISPLAY FUNCTION TRIM(EMPTY)
            ELSE
                DISPLAY " but it should be " SHOULD_BE
            END-IF.


