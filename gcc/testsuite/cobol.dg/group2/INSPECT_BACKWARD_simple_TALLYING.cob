       *> { dg-do run }
       *> { dg-options "-dialect mf" }
       *> { dg-output-file "group2/INSPECT_BACKWARD_simple_TALLYING.out" }

        program-id.         prog.
        data                division.
        working-storage     section.
        01 item pic x(64).
        01 counter pic 999.
        procedure division.

        move "AAXAAAYAAAAZAAAAA" to item
        display function trim(item)
        display "Forward:"

        move zero to counter
        inspect item tallying 
            counter for all "A" 
        display  "FOR ALL A                  " counter

        move zero to counter
        move "AAXAAAYAAAAZAAAAA" to item
        inspect item tallying 
            counter for all "A" after "X"
        display  "FOR ALL A after X          " counter
        
        move zero to counter
        move "AAXAAAYAAAAZAAAAA" to item
        inspect item tallying 
            counter for all "A" before "Z"
        display  "FOR ALL A before Z         " counter

        move zero to counter
        move "AAXAAAYAAAAZAAAAA" to item
        inspect item tallying 
            counter for all "A" after "X" before "Z"
        display  "FOR ALL A after X before Z " counter

        move zero to counter
        move "AAXAAAYAAAAZAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" to item
        inspect item tallying 
            counter for trailing "A"
        display  "FOR TRAILING A             " counter


        display "Backward:"
        move zero to counter
        inspect backward item tallying 
            counter for all "A" 
        display  "FOR ALL A                  " counter

        move zero to counter
        move "AAXAAAYAAAAZAAAAA" to item
        inspect backward item tallying 
            counter for all "A" after "X"
        display  "FOR ALL A after X          " counter
        
        move zero to counter
        move "AAXAAAYAAAAZAAAAA" to item
        inspect backward item tallying 
            counter for all "A" before "Z"
        display  "FOR ALL A before Z         " counter

        move zero to counter
        move "AAXAAAYAAAAZAAAAA" to item
        inspect backward item tallying 
            counter for all "A" after "Z" before "X"
        display  "FOR ALL A after Z before X " counter

        move zero to counter
        move "AAXAAAYAAAAZAAAAA" to item
        inspect backward item tallying 
            counter for trailing "A"
        display  "FOR TRAILING A             " counter

        goback.
        end program prog.

