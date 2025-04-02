       *> { dg-do run }
       *> { dg-output-file "group2/INSPECT_ISO_Example_6.out" }

        Identification Division.
        Program-Id. Clouseau.
        Data Division.
        Working-Storage Section.
        01 rows   pic 99 value 1.

        01 rowlim pic 99.
        01 ncount pic 99.

        01 inputs.
           05 row occurs  6 times indexed by counter.
              10 star  PIC X.
              10 input PIC X(20).
              10 output PIC X(20).
        77 len PIC 9(8).
        Procedure Division.
        *> Odd-numbered rows are "read only" and contain the inputs and expected
        *> outputs.
        *> Even-numbered rows are modified by the INSPECT statements and contain
        *> the observed outputs
        Move ' AC"AEBDFBCD#AB"D    AC"XEYXFYZX#AB"D' to row(1).
`
        compute rowlim = 2*rows - 1

        Display '  INPUT                OUTPUT'
        Display '  -------------------- ----------------'
        Perform Example-1 with test after
            varying counter from 1 by 2 until counter >= rowlim.

        Goback.

        Inspection Section.
        Example-1.
        Move row(counter) to row(counter + 1)

        Move function length( function trim(input(counter)) ) to len.
        MOVE INPUT(COUNTER) TO OUTPUT(COUNTER + 1)
        INSPECT OUTPUT(COUNTER + 1)(1:len) CONVERTING
            "ABCD" TO "XYZX" AFTER QUOTE BEFORE "#".

        If row(counter) = row(counter + 1) then
            Move '*' to star(counter + 1)
        Else
            Move '!' to star(counter + 1).

        Display star(counter)     ' '
                input(counter)   ' ' with no advancing
        display function trim(output(counter))

        Display  star(1 + counter)    ' '
                input(1 + counter)   ' ' with no advancing
        display function trim(output(1 + counter))
        continue.
        end program Clouseau.

