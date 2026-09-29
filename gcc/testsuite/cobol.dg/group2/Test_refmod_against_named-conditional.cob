      *> Do not edit this generated file.  See README.txt
      *> { dg-do run }
       *> { dg-options "-dialect mf" }
       *> { dg-output-file "group2/Test_refmod_against_named-conditional.out" }
        identification              division.
        program-id.                 prog.
        environment division.
        configuration section.
        special-names.
           class digit-or-hyphen '0' thru '9' '-'.
        data                        division.
        working-storage             section.
        01 foo       pic x(7)  value '123-x45'.
        procedure                   division.
            if foo(4:1) is digit-or-hyphen
                display "4 is okay"
            else
                display "4 is bad"
                end-if
            if foo(5:1) is digit-or-hyphen
                display "5 is bad"
            else
                display "5 is okay"
                end-if
            if foo(6:1) is digit-or-hyphen
                display "6 is okay"
            else
                display "6 is bad"
                end-if
            goback.
        end program                 prog.

