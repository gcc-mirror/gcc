C { dg-do compile }
C { dg-options "-fd-lines-as-code" }
C Verifies that column numbers are dealt with correctly when handling D lines.
C234567890
d     i = 0 ! this may not move to the left
d    1  + 1 ! this should be a continuation line
      goto 2345
d23450continue ! statement labels are correctly identified
      end
      
