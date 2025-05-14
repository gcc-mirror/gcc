       *> { dg-do run }
       *> { dg-output-file "group2/INSPECT_CONVERTING_TO_figurative_constants.out" }

        identification division.
        program-id. clouseau.
        data division.
        working-storage section.
        01 item pic x(12).
        procedure division.
        move all "abcd" to item
        inspect item converting "abcd" to low-values
        display "low-values " space """" item """"
        move all "abcd" to item
        inspect item converting "abcd" to spaces
        display "spaces     " space """" item """"
        move all "abcd" to item
        inspect item converting "abcd" to zeros
        display "zeros      " space """" item """"
        move all "abcd" to item
        inspect item converting "abcd" to quotes
        display "quotes     " space """" item """"
        move all "abcd" to item
        inspect item converting "abcd" to high-values
        display "high-values" space """" item """"
        goback.
        end program clouseau.

