       *> { dg-do run }
       *> { dg-output-file "group2/Complex_IF.out" }
        identification division.
        program-id. phonebook.
        data division.
        working-storage section.
        01 name1 pic x(10) value "one".
        01 name2 pic x(10) value "two".
        01 flag  pic x     value 'a'.
        procedure division.
        move 'l' to flag
        perform checkit
        goback.
        checkit.
            if (name1 = name2 and flag = "F" or "f" )
                or flag = "L" or "l"
            then
               display "the test is TRUE"
            else
               display "the test is FALSE"
            end-if.
        end program phonebook.

