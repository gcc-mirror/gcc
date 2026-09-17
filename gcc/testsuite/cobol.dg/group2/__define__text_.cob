      *> Do not edit this generated file.  See README.txt
      *> { dg-do run }
       *> { dg-options "-dialect mf" }
       *> { dg-output-file "group2/__define__text_.out" }
        identification              division.
        program-id.                 prog1.
        >> define defined_text as "Defined text"
        procedure                   division.
            display defined_text
            call "prog2" using defined_text
            goback.
        end program                 prog1.
        
        identification              division.
        program-id.                 prog2.
        data                        division.
        linkage                     section.
        01 display_text pic X ANY LENGTH.
        procedure                   division using display_text.
            display display_text
            goback.
        end program                 prog2.

