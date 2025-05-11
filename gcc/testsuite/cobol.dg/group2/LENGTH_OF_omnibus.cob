       *> { dg-do run }
       *> { dg-options "-dialect ibm" }
       *> { dg-output-file "group2/LENGTH_OF_omnibus.out" }

        program-id. prog.
        data division.
        working-storage section.
        01      desc1.
         05     desc1-entry pic x(5) occurs 10.

        01      desc2.
         05     desc2-table occurs 10 times.
          10    desc2-entry pic x(5).

        01      desc3.
         05     desc3-outer occurs 1 to 5 times depending on desc3-lim.
          10    desc3-outer-txt   pic x(7).
          10    desc3-inner occurs 11 times.
           15   desc3-inner-text  pic x(13).
        77 desc3-lim binary-long.

        77      msg pic x(64).
        77      should-be pic zzzz9.
        77      but-is    pic zzzz9.

        procedure division.

        display "using LENGTH OF"

        move    "Length of desc1" to msg
        move    50 to should-be
        move    length of desc1 to but-is
        perform result-is

        move    "Length of desc1-entry" to msg
        move    5 to should-be
        move    length of desc1-entry to but-is
        perform result-is

        move    "Length of desc1-entry(1)" to msg
        move    5 to should-be
        move    length of desc1-entry(1) to but-is
        perform result-is

        move    "Length of desc2" to msg
        move    50 to should-be
        move    length of desc2 to but-is
        perform result-is

        move    "Length of desc2-table" to msg
        move    5 to should-be
        move    length of desc2-table to but-is
        perform result-is

        move    "Length of desc2-entry" to msg
        move    5 to should-be
        move    length of desc2-entry to but-is
        perform result-is

        move    "Length of desc2-entry(1)" to msg
        move    5 to should-be
        move    length of desc2-entry(1) to but-is
        perform result-is

        move    5 to desc3-lim

        move    "Length of desc3" to msg
        move    750 to should-be
        move    length of desc3 to but-is
        perform result-is

        move    "Length of desc3-outer" to msg
        move    150 to should-be
        move    length of desc3-outer to but-is
        perform result-is

        move    "Length of desc3-outer(1)" to msg
        move    150 to should-be
        move    length of desc3-outer(1) to but-is
        perform result-is

        move    "Length of desc3-outer-txt" to msg
        move    7 to should-be
        move    length of desc3-outer-txt to but-is
        perform result-is

        move    "Length of desc3-inner" to msg
        move    13 to should-be
        move    length of desc3-inner to but-is
        perform result-is

        move    "Length of desc3-inner(1)" to msg
        move    13 to should-be
        move    length of desc3-inner(1) to but-is
        perform result-is

        goback.
        result-is.
        display function trim(msg) ": " with no advancing
        if but-is equal to should-be
            display function trim(but-is)
        else
            display "should be " function trim(should-be)
                    " but is "   function trim(but-is)
        end-if.
        end program prog.

