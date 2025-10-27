       *> { dg-do run }
       *> { dg-output-file "group2/Simple_p-scaling.out" }

        identification      division.
        program-id.         prog.
        data                division.
        working-storage     section.
        01  vars.
         05 vars01    picture 99ppp   DISPLAY        value 78000 .
         05 vars02    picture 99ppp   BINARY         value 78000 .
         05 vars03    picture 99ppp   COMP-3         value 78000 .
         05 vars04    picture 99ppp   COMP-5         value 78000 .
         05 vars05    picture 99ppp   PACKED-DECIMAL value 78000 .
        01  vary.
         05 vary01    picture ppp99   DISPLAY        value 0.00078 .
         05 vary02    picture ppp99   BINARY         value 0.00078 .
         05 vary03    picture ppp99   COMP-3         value 0.00078 .
         05 vary04    picture ppp99   COMP-5         value 0.00078 .
         05 vary05    picture ppp99   PACKED-DECIMAL value 0.00078 .
        procedure           division.
        display vars01 
        display vars02 
        display vars03 
        display vars04 
        display vars05 
        display vary01 
        display vary02 
        display vary03 
        display vary04 
        display vary05 
        goback.
        end program         prog.

