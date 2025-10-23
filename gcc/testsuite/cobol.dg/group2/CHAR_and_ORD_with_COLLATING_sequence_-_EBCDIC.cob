       *> { dg-do run }
       *> { dg-options "-finternal-ebcdic" }
       *> { dg-output-file "group2/CHAR_and_ORD_with_COLLATING_sequence_-_EBCDIC.out" }
        IDENTIFICATION      DIVISION.
        PROGRAM-ID.         prog.
        ENVIRONMENT         DIVISION.
        CONFIGURATION       SECTION.
        OBJECT-COMPUTER.
            GNU-Linux
            PROGRAM COLLATING SEQUENCE IS THE-WILD-ONE.
        SPECIAL-NAMES.
            ALPHABET
            THE-WILD-ONE IS "A" THRU "H" "I" ALSO "J", ALSO "K", ALSO
            "L" ALSO "M" ALSO "N" "O" THROUGH "Z" "0" THRU "9".
        PROCEDURE           DIVISION.
        DISPLAY LOW-VALUE
        DISPLAY HIGH-VALUE
        DISPLAY FUNCTION CHAR(1).
        DISPLAY FUNCTION CHAR(9).
        DISPLAY FUNCTION CHAR(10).
        DISPLAY FUNCTION ORD("A")
        DISPLAY FUNCTION ORD("I")
        DISPLAY FUNCTION ORD("J")
        DISPLAY FUNCTION ORD("K")
        DISPLAY FUNCTION ORD("O")
        GOBACK.

