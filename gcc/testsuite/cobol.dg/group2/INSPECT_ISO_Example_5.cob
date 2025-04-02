       *> { dg-do run }
       *> { dg-output-file "group2/INSPECT_ISO_Example_5.out" }
        Identification Division.
        Program-Id. Clouseau.
        *> Note:  Although modeled on Example-5 in Appendix D of the ISO 2023
        *> specification, all six of the samples are incorrect.
        *> This code executes the examples as written, and the test suite checks
        *> For the answers believed to be correct
        Data Division.
        Working-Storage Section.
        01 item-1 pic x(8) value "ABABBCAB".
        01 item-2 pic x(7) value "ABDBABC".
        01 item-3 pic x(8) value "BCABCABD".
        01 count-0 pic 9 value zero.
        01 count-1 pic 9 value zero.
        01 count-2 pic 9 value zero.
        Procedure Division.

        initialize item-1 item-2 item-3 count-0 count-1 count-2 all value
        display item-1 "   " with no advancing
        INSPECT BACKWARD ITEM-1 TALLYING
            COUNT-0 FOR ALL "AB" BEFORE "BC"
            COUNT-1 FOR LEADING "B"
            COUNT-2 FOR CHARACTERS AFTER "A" BEFORE "C"
        INSPECT BACKWARD ITEM-1 REPLACING
            ALL "AB" BY "XY" BEFORE "BC"
            LEADING "B" BY "V" AFTER "D"
        display count-0 space count-1 space count-2 space item-1

        initialize item-1 item-2 item-3 count-0 count-1 count-2 all value
        display item-2 "    " with no advancing
        INSPECT BACKWARD ITEM-2 TALLYING
            COUNT-0 FOR ALL "AB" BEFORE "BC"
            COUNT-1 FOR LEADING "B"
            COUNT-2 FOR CHARACTERS AFTER "A" BEFORE "C"
        INSPECT BACKWARD ITEM-2 REPLACING
            ALL "AB" BY "XY" BEFORE "BC"
            LEADING "B" BY "V" AFTER "D"
        display count-0 space count-1 space count-2 space item-2

        initialize item-1 item-2 item-3 count-0 count-1 count-2 all value
        display item-3 "   " with no advancing
        INSPECT BACKWARD ITEM-3 TALLYING
            COUNT-0 FOR ALL "AB" BEFORE "BC"
            COUNT-1 FOR LEADING "B"
            COUNT-2 FOR CHARACTERS AFTER "A" BEFORE "C"
        INSPECT BACKWARD ITEM-3 REPLACING
            ALL "AB" BY "XY" BEFORE "BC"
            LEADING "B" BY "V" AFTER "D"
        display count-0 space count-1 space count-2 space item-3

        initialize item-1 item-2 item-3 count-0 count-1 count-2 all value
        MOVE FUNCTION REVERSE (ITEM-1) TO ITEM-1
        display item-1 "   " with no advancing
        INSPECT ITEM-1 TALLYING
        COUNT-0 FOR ALL "AB" BEFORE "BC"
        COUNT-1 FOR LEADING "B"
        COUNT-2 FOR CHARACTERS AFTER "A" BEFORE "C"
        INSPECT BACKWARD ITEM-1 REPLACING
        ALL "AB" BY "XY" BEFORE "BC"
        LEADING "B" BY "V" AFTER "D"
        display count-0 space count-1 space count-2 space item-1

        initialize item-1 item-2 item-3 count-0 count-1 count-2 all value
        MOVE FUNCTION REVERSE (ITEM-2) TO ITEM-2
        display item-2 "    " with no advancing
        INSPECT ITEM-2 TALLYING
        COUNT-0 FOR ALL "AB" BEFORE "BC"
        COUNT-1 FOR LEADING "B"
        COUNT-2 FOR CHARACTERS AFTER "A" BEFORE "C"
        INSPECT BACKWARD ITEM-2 REPLACING
        ALL "AB" BY "XY" BEFORE "BC"
        LEADING "B" BY "V" AFTER "D"
        display count-0 space count-1 space count-2 space item-2

        initialize item-1 item-2 item-3 count-0 count-1 count-2 all value
        MOVE FUNCTION REVERSE (ITEM-3) TO ITEM-3
        display item-3 "   " with no advancing
        INSPECT ITEM-3 TALLYING
        COUNT-0 FOR ALL "AB" BEFORE "BC"
        COUNT-1 FOR LEADING "B"
        COUNT-2 FOR CHARACTERS AFTER "A" BEFORE "C"
        INSPECT BACKWARD ITEM-3 REPLACING
        ALL "AB" BY "XY" BEFORE "BC"
        LEADING "B" BY "V" AFTER "D"
        display count-0 space count-1 space count-2 space item-3

        goback.
        end program Clouseau.

