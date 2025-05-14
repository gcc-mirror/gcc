       *> { dg-do run }
       *> { dg-output-file "group2/FUNCTION_HEX-OF.out" }

        IDENTIFICATION   DIVISION.
        PROGRAM-ID.      prog.
        DATA             DIVISION.
        WORKING-STORAGE  SECTION.
        01 PAC PIC 9(5) COMP-3 VALUE 12345.
        PROCEDURE        DIVISION.
            DISPLAY FUNCTION HEX-OF('Hello, world!')
            DISPLAY FUNCTION HEX-OF(PAC).
            END PROGRAM prog.

