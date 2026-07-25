      *> Do not edit this generated file.  See README.txt
      *> { dg-do run }
       *> { dg-output-file "group2/MOVE__identical__operations.out" }
        program-id. bugger.
        data division.
        working-storage section.
        01 debug-contents-e pic x(76) external.
        01 debug-contents-b pic x(76) based   .
        01 debug-contents-w pic x(76)         .
        procedure division.
            allocate debug-contents-b.
            move "telephone-1" to debug-contents
            move debug-contents to debug-contents-b
            move debug-contents-b to debug-contents-e
            move debug-contents-e to debug-contents-w
            display "start with " function trim(debug-contents-e)
            call "bug" using debug-contents-w
            display "end   with " function trim(debug-contents-e)
            goback.
        end program bugger.
      *>
        program-id. bug.
        data division.
        working-storage section.
        01 debug-contents-e pic x(76) external.
        01 debug-contents-w2 pic x(76).
        linkage section.
        01  dc.
          10 debug-contents-l pic x(76).
        procedure division using dc.
           move debug-contents-l to debug-contents-w2
           string function trim(debug-contents-w2) " - modified" into debug-contents-e
           goback.

