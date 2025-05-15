       *> { dg-do run }
       *> { dg-output-file "group2/INSPECT_ISO_Example_3.out" }

        Identification Division.
        Program-Id. Clouseau.
        Data Division.
        Working-Storage Section.
        01 inputs.
           05 row occurs 10 times indexed by counter.
              10 star  PIC X.
              10 input PIC X(20).
              10 count PIC 99 occurs 3 times.
              10 output PIC X(20).
        77 len PIC 9(8).

        Procedure Division.
        Move ' BBEABDABABBCABE     030002BBEXYZXYXYZCABV' to row(1).
        Move ' ADDDDC              000004AZZZZC'  to row(3).
        Move ' ADDDDA              000005AZZZZZ'  to row(5).
        Move ' CDDDDC              000000CDDDDC'  to row(7).
        Move ' BDBBBDB             000300BDWWWDB' to row(9).

        Display '  INPUT                C0 C1 C2 OUTPUT'
        Display '  -------------------- -- -- -- --------------------'
        Perform Example-3 with test after
            varying counter from 1 by 2 until counter = 9.

        Goback.

        Inspection Section.
        Example-3.
        Move row(counter) to row(counter + 1)
        Move input(counter) to output(counter)
        Move Zero to count(counter 1)
        Move Zero to count(counter 2)
        Move Zero to count(counter 3)

        Move function length( function trim(input(counter)) ) to len.
        INSPECT OUTPUT(COUNTER)(1:len) TALLYING
             COUNT(counter 1) FOR ALL "AB" BEFORE "BC"
             COUNT(counter 2) FOR LEADING "B" AFTER "D"
             COUNT(counter 3) FOR CHARACTERS AFTER "A" BEFORE "C";
        INSPECT OUTPUT(COUNTER)(1:len) REPLACING
             ALL "AB" BY "XY" BEFORE "BC"
             LEADING "B" BY "W" AFTER "D"
             FIRST "E" BY "V" AFTER "D"
             CHARACTERS BY "Z" AFTER "A" BEFORE "C"

       If row(counter) = row(counter + 1) then
          Move '*' to star(counter + 1)
       Else
          Move '!' to star(counter + 1).

       Display star(counter)   ' '
                input(counter)   ' '
                count(counter 1) ' '
                count(counter 2) ' '
                count(counter 3) ' '
               function trim(output(counter))
        Display star(1 + counter)   ' '
                input(1 + counter)   ' '
                count(1 + counter 1) ' '
                count(1 + counter 2) ' '
                count(1 + counter 3) ' '
               function trim(output(1 + counter))
        continue.
        end program Clouseau.

