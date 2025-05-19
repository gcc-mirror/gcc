       *> { dg-do run }
       *> { dg-output-file "group2/INSPECT_ISO_Example_4.out" }

        Identification Division.
        Program-Id. Clouseau.
        Data Division.
        Working-Storage Section.
        01 rows   pic 99 value 1.
        01 counts pic 99 value 1.

        01 rowlim pic 99.
        01 ncount pic 99.

        01 inputs.
           05 row occurs  2 times indexed by counter.
              10 star  PIC X.
              10 input PIC X(20).
              10 count PIC 99 occurs 1 times.
              10 output PIC X(20).
        77 len PIC 9(8).
        Procedure Division.
        *> Odd-numbered rows are "read only" and contain the inputs and expected
        *> outputs.
        *> Even-numbered rows are modified by the INSPECT statements and contain
        *> the observed outputs
        Move ' ABABABABC           01ABABXYABC' to row(1).

        compute rowlim = 2*rows - 1

        Display '  INPUT                C0 C1 OUTPUT'
        Display '  -------------------- -- -- ----------------'
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
        INSPECT INPUT(COUNTER)(1:len) TALLYING
            COUNT(counter + 1 1) FOR ALL "AB" AFTER "BA" BEFORE "BC";
        INSPECT OUTPUT(COUNTER + 1)(1:len) REPLACING
            ALL "AB" BY "XY" AFTER "BA" BEFORE "BC"
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

