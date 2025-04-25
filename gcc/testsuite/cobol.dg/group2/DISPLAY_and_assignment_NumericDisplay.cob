       *> { dg-do run }
       *> { dg-output-file "group2/DISPLAY_and_assignment_NumericDisplay.out" }
        identification      division.
        program-id.         prog.
        data                division.
        working-storage     section.
        01  vars.
         05 vars-display-1.
          10 var01a         pic  99v999                     display       value   54.321    .
          10 var01b         pic s99v999                     display       value   54.321    .
          10 var01c         pic s99v999  leading            display       value  -54.321    .
          10 var01d         pic s99v999  trailing           display       value   54.321    .
          10 var01e         pic s99v999  leading  separate  display       value  -54.321    .
          10 var01f         pic s99v999  trailing separate  display       value   54.321    .
         05 vars-display-2.
          10 var01g         pic  9999ppp                    display       value   4321000   .
          10 var01h         pic s9999ppp                    display       value   4321000   .
          10 var01i         pic s9999ppp  leading           display       value  -4321000   .
          10 var01j         pic s9999ppp  trailing          display       value   4321000   .
          10 var01k         pic s9999ppp  leading  separate display       value  -4321000   .
          10 var01l         pic s9999ppp  trailing separate display       value   4321000   .
         05 vars-display-3.
          10 var01m         pic  ppp9999                    display       value   .0001234  .
          10 var01n         pic sppp9999                    display       value   .0001234  .
          10 var01o         pic sppp9999  leading           display       value  -.0001234  .
          10 var01p         pic sppp9999  trailing          display       value   .0001234  .
          10 var01q         pic sppp9999  leading  separate display       value  -.0001234  .
          10 var01r         pic sppp9999  trailing separate display       value   .0001234  .
        procedure           division.
        display var01a
        display var01b
        display var01c
        display var01d
        display var01e
        display var01f
        display var01g
        display var01h
        display var01i
        display var01j
        display var01k
        display var01l
        display var01m
        display var01n
        display var01o
        display var01p
        display var01q
        display var01r

        move  12.345 to var01a var01c var01e
        move -12.345 to var01b var01d var01f

        move  9876000 to var01g var01i var01k
        move -9876000 to var01h var01j var01l

        move  .0006789 to var01m var01o var01q
        move -.0006789 to var01n var01p var01r

        display var01a
        display var01b
        display var01c
        display var01d
        display var01e
        display var01f
        display var01g
        display var01h
        display var01i
        display var01j
        display var01k
        display var01l
        display var01m
        display var01n
        display var01o
        display var01p
        display var01q
        display var01r

        continue.
        quit.
        goback.
        end program         prog.


