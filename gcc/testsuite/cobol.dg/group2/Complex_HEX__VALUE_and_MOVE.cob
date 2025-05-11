       *> { dg-do run }
       *> { dg-output-file "group2/Complex_HEX__VALUE_and_MOVE.out" }

        identification division.
        program-id. hex-init.
        data division.
        working-storage section.
        01  var-01020304.
            05 filler1.
                10 filler2      pic x(2) VALUE "33".
                10 as-value     pic x(4) VALUE  X'01020304'.
                10 filler3      pic x(2) VALUE "33".
            05 as-pointer redefines filler1 usage pointer.

        01  var-low.
            05 filler1.
                10 filler2      pic x(2) VALUE "33".
                10 as-value     pic x(4) VALUE  LOW-VALUES.
                10 filler3      pic x(2) VALUE "33".
            05 as-pointer redefines filler1 usage pointer.
        01  var-space.
            05 filler1.
                10 filler2      pic x(2) VALUE "33".
                10 as-value     pic x(4) VALUE  SPACE.
                10 filler3      pic x(2) VALUE "33".
            05 as-pointer redefines filler1 usage pointer.
        01  var-quote.
            05 filler1.
                10 filler2      pic x(2) VALUE "33".
                10 as-value     pic x(4) VALUE  QUOTE.
                10 filler3      pic x(2) VALUE "33".
            05 as-pointer redefines filler1 usage pointer.
        01  var-zero.
            05 filler1.
                10 filler2      pic x(2) VALUE "33".
                10 as-value     pic x(4) VALUE  ZERO.
                10 filler3      pic x(2) VALUE "33".
            05 as-pointer redefines filler1 usage pointer.
        01  var-high.
            05 filler1.
                10 filler2      pic x(2) VALUE "33".
                10 as-value     pic x(4) VALUE  HIGH-VALUES.
                10 filler3      pic x(2) VALUE "33".
            05 as-pointer redefines filler1 usage pointer.
        01  move-target.
            05 filler1.
                10 filler2      pic x(2) VALUE "33".
                10 as-value     pic x(4) VALUE "3333".
                10 filler3      pic x(2) VALUE "33".
            05 as-pointer redefines filler1 usage pointer.
        procedure division.
        display "the value is    " as-pointer of var-01020304.
        display "should be       0x3333040302013333"
        display "var-low  :      " as-pointer of var-low
        display "var-space:      " as-pointer of var-space
        display "var-quote:      " as-pointer of var-quote
        display "var-zero :      " as-pointer of var-zero
        display "var-high :      " as-pointer of var-high
        display "initial         " as-pointer of move-target
        move low-value to as-value of move-target
        display "low-value       " as-pointer of move-target
        move space to as-value of move-target
        display "space           " as-pointer of move-target
        move quote to as-value of move-target
        display "quote           " as-pointer of move-target
        move zeroes to as-value of move-target
        display "zeroes          " as-pointer of move-target
        move high-value to as-value of move-target
        display "high-value      " as-pointer of move-target
        move X'01020304' to as-value of move-target
        display "01020304        " as-pointer of move-target
        move "33333333" to move-target
        move X'00' to filler3 of move-target(1:1)
        display "ref-mod         " as-pointer of move-target
        stop run.

