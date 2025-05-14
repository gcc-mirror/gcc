       *> { dg-do run }
       *> { dg-output-file "group2/Recursive_FUNCTION_with_local-storage.out" }
        IDENTIFICATION   DIVISION.
        FUNCTION-ID.      callee.
        DATA             DIVISION.
        LOCAL-STORAGE    SECTION.
        01 LCL-X         PIC 999 .
        LINKAGE          SECTION.
        01 parm          PIC 999.
        01 retval        PIC 999.
        PROCEDURE        DIVISION USING parm RETURNING retval.
            display "On entry, parm is: " parm
            move parm to lcl-x
            move parm to retval
            subtract 1 from parm
            if parm > 0
                display "A The function returns " function callee(parm).
            if lcl-x not equal to retval
                display "On exit, lcl-s and retval are: " lcl-x " and " retval
                display "But they should be equal to each other"
                end-if
            goback.
            end function callee.
        IDENTIFICATION   DIVISION.
        PROGRAM-ID.      caller.
        ENVIRONMENT      DIVISION.
        CONFIGURATION    SECTION.
        REPOSITORY.
                         FUNCTION callee.
        DATA             DIVISION.
        WORKING-STORAGE  SECTION.
        01 val           PIC 999 VALUE 5.
        PROCEDURE        DIVISION.
           DISPLAY "Starting value is: " val
           display "B The function returns " function callee(val).
           STOP RUN.
           end program caller.

