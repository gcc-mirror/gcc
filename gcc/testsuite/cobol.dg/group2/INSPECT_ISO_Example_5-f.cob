       *> { dg-do run }
       *> { dg-output-file "group2/INSPECT_ISO_Example_5-f.out" }

        Identification Division.
        Program-Id. Clouseau.
        *> Note:  Although modeled on Example-5 in Appendix D of the ISO 2023
        *> specification, all three of the samples are incorrect.  This code
        *> modifies the problem to make it somewhat more interesting, and, of
        *> course, changes the answers so that they are correct for the problem.
        Data Division.
        Working-Storage Section.
        01 rows   pic 99 value 3.
        01 counts pic 99 value 3.

        01 rowlim pic 99.
        01 ncount pic 99.

        01 inputs.
           05 row occurs  6 times indexed by counter.
              10 star  PIC X.
              10 input PIC X(20).
              10 count PIC 99 occurs 3 times.
              10 output PIC X(20).
        77 len PIC 9(8).
        Procedure Division.
        *> Odd-numbered rows are "read only" and contain the inputs and expected
        *> outputs.
        *> Even-numbered rows are modified by the INSPECT statements and contain
        *> the observed outputs
        Move ' ABABBCAB            000106ABABBCXY' to row(1).
        Move ' ABDBABC             000001AVDBABC'  to row(3).
        Move ' BCABCABD            010000BCABCAVD' to row(5).
`
        compute rowlim = 2*rows - 1

        Display '  INPUT                C0 C1 C2 OUTPUT'
        Display '  -------------------- -- -- -- ----------------'
        Perform Example-1 with test after
            varying counter from 1 by 2 until counter >= rowlim.

        Goback.

        Inspection Section.
        Example-1.
        Move row(counter) to row(counter + 1)
        
        perform varying ncount from 1 by 1 until ncount > counts
            Move Zero to count(counter + 1 ncount)
            end-perform

        Move function length( function trim(input(counter)) ) to len.
        MOVE INPUT(COUNTER) TO OUTPUT(COUNTER + 1)
        INSPECT BACKWARD INPUT(COUNTER)(1:len) TALLYING
            COUNT(counter + 1 1) FOR ALL "AB" BEFORE "BC"
            COUNT(counter + 1 2) FOR LEADING "B"
            COUNT(counter + 1 3) FOR CHARACTERS AFTER "A" BEFORE "D"
        INSPECT BACKWARD OUTPUT(COUNTER + 1)(1:len) REPLACING
            ALL "AB" BY "XY" BEFORE "BC"
            LEADING "B" BY "V" AFTER "D"

        If row(counter) = row(counter + 1) then
            Move '*' to star(counter + 1)
        Else
            Move '!' to star(counter + 1).

        Display star(counter)     ' '
                input(counter)   ' ' with no advancing
        perform varying ncount from 1 by 1 until ncount > counts
            Display count(counter ncount) ' ' with no advancing
            end-perform
        display function trim(output(counter))

        Display  star(1 + counter)    ' '
                input(1 + counter)   ' ' with no advancing
        perform varying ncount from 1 by 1 until ncount > counts
            Display count(1 + counter ncount) ' ' with no advancing
            end-perform
        display function trim(output(1 + counter))
        continue.
        end program Clouseau.

