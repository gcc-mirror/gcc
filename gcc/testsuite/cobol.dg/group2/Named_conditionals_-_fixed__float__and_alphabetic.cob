       *> { dg-do run }
       *> { dg-output-file "group2/Named_conditionals_-_fixed__float__and_alphabetic.out" }
        identification      division.
        program-id.         prog.
        data                division.
        working-storage     section.
        01  makeofcar        pic x(10).
            88 volksgroup  value "skoda", "seat",
                                 "audi", "volkswagen"
                           false "boat".
            88 germanmade  value "volkswagen", "audi",
                                 "mercedes", "bmw",
                                 "porsche".        
        01  agegroup  pic 999.
            88 child  value  0 through  12.
            88 teen   value 13 through  19.
            88 adult  value 20 through 999.
        01  floats  float-long.
            88 neg   value -1 through -.1 .
            88 zed   value zero           .
            88 pos   value .1 through 1.0 .
        procedure           division.
        move "ford" to makeofcar
        display function trim (makeofcar)
        if volksgroup display "  volksgroup" end-if
        if germanmade display "  germanmade" end-if
        move "skoda" to makeofcar
        display function trim (makeofcar)
        if volksgroup display "  volksgroup" end-if
        if germanmade display "  germanmade" end-if
        move "volkswagen" to makeofcar
        display function trim (makeofcar)
        if volksgroup display "  volksgroup" end-if
        if germanmade display "  germanmade" end-if
        move    5 to agegroup.
        display agegroup with no advancing
        if child display " child" end-if
        if teen  display " teen" end-if
        if adult display " adult" end-if
        move    15 to agegroup.
        display agegroup with no advancing
        if child display " child" end-if
        if teen  display " teen" end-if
        if adult display " adult" end-if
        move    75 to agegroup.
        display agegroup with no advancing
        if child display " child" end-if
        if teen  display " teen" end-if
        if adult display " adult" end-if
        move -0.5 to floats
        display floats with no advancing
        if neg  display " minus" end-if
        if zed  display " zero"  end-if
        if pos  display " plus"  end-if
        move zero to floats
        display floats with no advancing
        if neg  display " minus" end-if
        if zed  display " zero"  end-if
        if pos  display " plus"  end-if
        move 0.5 to floats
        display floats with no advancing
        if neg  display " minus" end-if
        if zed  display " zero"  end-if
        if pos  display " plus"  end-if
        continue.
        quit.
        goback.
        end program         prog.

