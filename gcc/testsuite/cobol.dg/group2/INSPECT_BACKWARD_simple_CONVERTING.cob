       *> { dg-do run }
       *> { dg-options "-dialect mf" }
       *> { dg-output-file "group2/INSPECT_BACKWARD_simple_CONVERTING.out" }

        program-id.         prog.
        data                division.
        working-storage     section.
        01 item             pic x(64).
        01 should-be        pic x(64).
        procedure division.
        display "Forward:"
        move "the quick brown fox jumps over the lazy dog" to item
        inspect item converting 
                "abcdefghijklmnopqrstuvwxyz"
            TO  "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        move "THE QUICK BROWN FOX JUMPS OVER THE LAZY DOG" to should-be
        perform reportt

        move "the quick brown fox jumps over the lazy dog" to item
        inspect item converting 
                "abcdefghijklmnopqrstuvwxyz"
            TO  "ABCDEFGHIJKLMNOPQRSTUVWXYZ" before "jumps"
        move "THE QUICK BROWN FOX jumps over the lazy dog" to should-be
        perform reportt

        move "the quick brown fox jumps over the lazy dog" to item
        inspect item converting 
                "abcdefghijklmnopqrstuvwxyz"
            TO  "ABCDEFGHIJKLMNOPQRSTUVWXYZ" before "nothing"
        move "THE QUICK BROWN FOX JUMPS OVER THE LAZY DOG" to should-be
        perform reportt

        move "the quick brown fox jumps over the lazy dog" to item
        inspect item converting 
                "abcdefghijklmnopqrstuvwxyz"
            TO  "ABCDEFGHIJKLMNOPQRSTUVWXYZ" after "fox"
        move "the quick brown fox JUMPS OVER THE LAZY DOG" to should-be
        perform reportt

        move "the quick brown fox jumps over the lazy dog" to item
        inspect item converting 
                "abcdefghijklmnopqrstuvwxyz"
            TO  "ABCDEFGHIJKLMNOPQRSTUVWXYZ" after "fox" before "over"
        move "the quick brown fox JUMPS over the lazy dog" to should-be
        perform reportt

        move "the quick brown fox jumps over the lazy dog" to item
        inspect item converting 
                "abcdefghijklmnopqrstuvwxyz"
            TO  "ABCDEFGHIJKLMNOPQRSTUVWXYZ" after "fox" before "xyzzy"
        move "the quick brown fox JUMPS OVER THE LAZY DOG" to should-be
        perform reportt

        display "Reverse:"

        move "the quick brown fox jumps over the lazy dog" to item
        inspect backward item converting 
                "abcdefghijklmnopqrstuvwxyz"
            TO  "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        move "THE QUICK BROWN FOX JUMPS OVER THE LAZY DOG" to should-be
        perform reportt

        move "the quick brown fox jumps over the lazy dog" to item
        inspect backward item converting 
                "abcdefghijklmnopqrstuvwxyz"
            TO  "ABCDEFGHIJKLMNOPQRSTUVWXYZ" before "jumps"
        move "the quick brown fox jumps OVER THE LAZY DOG" to should-be
        perform reportt

        move "the quick brown fox jumps over the lazy dog" to item
        inspect backward item converting 
                "abcdefghijklmnopqrstuvwxyz"
            TO  "ABCDEFGHIJKLMNOPQRSTUVWXYZ" before "nothing"
        move "THE QUICK BROWN FOX JUMPS OVER THE LAZY DOG" to should-be
        perform reportt

        move "the quick brown fox jumps over the lazy dog" to item
        inspect backward item converting 
                "abcdefghijklmnopqrstuvwxyz"
            TO  "ABCDEFGHIJKLMNOPQRSTUVWXYZ" after "fox"
        move "THE QUICK BROWN fox jumps over the lazy dog" to should-be
        perform reportt

        move "the quick brown fox jumps over the lazy dog" to item
        inspect backward item converting 
                "abcdefghijklmnopqrstuvwxyz"
            TO  "ABCDEFGHIJKLMNOPQRSTUVWXYZ" before "fox" after "over"
        move "the quick brown fox JUMPS over the lazy dog" to should-be
        perform reportt

        move "the quick brown fox jumps over the lazy dog" to item
        inspect backward item converting 
                "abcdefghijklmnopqrstuvwxyz"
            TO  "ABCDEFGHIJKLMNOPQRSTUVWXYZ" before "xyzzy" after "over"
        move "THE QUICK BROWN FOX JUMPS over the lazy dog" to should-be
        perform reportt

        goback.
        reportt.
            display "                 " function trim(item)
        if item not equal to should-be
            display "should have been " function trim(should-be)
        end-if.
        end program prog.

