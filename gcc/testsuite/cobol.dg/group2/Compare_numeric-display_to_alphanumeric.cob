      *> Do not edit this generated file.  See README.txt
      *> { dg-do run }
       *> { dg-options "-dialect mf" }
       *> { dg-output-file "group2/Compare_numeric-display_to_alphanumeric.out" }
        identification      division.
        program-id.         prog.
        data                division.
        working-storage     section.
        01 zipcode                    pic 9(5).
        01 zipcodex redefines zipcode pic x(5).
        procedure           division.
            move space to zipcodex
            if zipcode equal space 
                display "space"
            else
                display "NOT space (BUG!)" end-if
            goback.
        end program         prog.

