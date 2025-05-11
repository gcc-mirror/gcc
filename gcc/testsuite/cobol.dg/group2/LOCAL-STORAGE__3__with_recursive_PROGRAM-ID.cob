       *> { dg-do run }
       *> { dg-output-file "group2/LOCAL-STORAGE__3__with_recursive_PROGRAM-ID.out" }

        IDENTIFICATION   DIVISION.
        PROGRAM-ID.      caller.
        PROCEDURE        DIVISION.
           CALL "callee"
           END-CALL.
           STOP RUN.
           end program caller.

        IDENTIFICATION   DIVISION.
        PROGRAM-ID.      callee.
        DATA             DIVISION.
        WORKING-STORAGE  SECTION.
        01 WRK-X         PIC 999 VALUE 5.
        LOCAL-STORAGE    SECTION.
        01 LCL-X         PIC 999 .
        PROCEDURE        DIVISION.
            display "On entry: " wrk-x
            move wrk-x to lcl-x
            subtract 1 from wrk-x
            if wrk-x > 0
                call "callee".
            display "On exit: " lcl-x
            goback.
            end program callee.

