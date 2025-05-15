       *> { dg-do run }
       *> { dg-output-file "group2/INSPECT_CONVERTING_TO_figurative_constants.out" }
        identification division.
        program-id. clouseau.
        data division.
        working-storage section.
        01 item pic x(12).
        01 pitem redefines item pointer.
        procedure division.
        move all "abcd" to item
        inspect item converting "abcd" to low-values
        display "low-values " space """" pitem """"
        move all "abcd" to item
        inspect item converting "abcd" to spaces
        display "spaces     " space """" pitem """"
        move all "abcd" to item
        inspect item converting "abcd" to zeros
        display "zeros      " space """" pitem """"
        move all "abcd" to item
        inspect item converting "abcd" to quotes
        display "quotes     " space """" pitem """"
        move all "abcd" to item
        inspect item converting "abcd" to high-values
        display "high-values" space """" pitem """"
        goback.
        end program clouseau.

