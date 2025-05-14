       *> { dg-do run }

        identification division.
        program-id. test.
        data division.
        working-storage section.
        01 datev    pic 99999999.
        01 should_be pic 9999.
        01 result    pic 9999.
        01 date-integer PIC 999999.
        01 i PIC 999.
        01 datex PIC X(8).
        01 xone    PIC  X.
        01 yyyydddv .
            02 yyyy   PIC 9999.
            02 filler PIC X VALUE "-".
            02 ddd    PIC 999.
        procedure division.
      *>    TESTING YYYYMMDD
        move "19000229" to datex
        move function TEST-FORMATTED-DATETIME("YYYYMMDD", datex) to RESULT
        move 8 to should_be
        if result not equal to should_be then
            display "TEST-FORMATTED-DATETIME("
                    """YYYYMMDD"""
                    ", "
                    function trim(datex)
                    ") should have been "
                    should_be " but was " result
            move 1 to return-code
            end-if.
      *>    Test an entire year of YYYYMMDD:
        move function integer-of-date(19880101) to date-integer
        perform until date-integer >= function integer-of-date(19890101)
            move function date-of-integer(date-integer) to datev
            move function TEST-FORMATTED-DATETIME("YYYYMMDD", datev) to RESULT
            move zero to should_be
            if result not equal to should_be then
                display "TEST-FORMATTED-DATETIME("
                        """YYYYMMDD"""
                        ", "
                        datev
                        ") should have been "
                        should_be " but was " result
                move 1 to return-code
                end-if
            add 1 to date-integer
            end-perform.
      *> Make sure foreign characters trigger the correct gazinga in YYYYMMDD
        move "19530227" to datex
        perform varying i from 1 by 1 until i > 8
            move datex(i:1) to xone
            move 'X' to datex(i:1)
            move function TEST-FORMATTED-DATETIME("YYYYMMDD", datex) to RESULT
            move i to should_be
            if result not equal to should_be then
                display "TEST-FORMATTED-DATETIME("
                        """YYYYMMDD"""
                        ", "
                        function trim(datex)
                        ") should have been "
                        should_be " but was " result
                move 1 to return-code
                end-if
            move xone to datex(i:1)
            end-perform.
        move "19000229" to datex
        move function TEST-FORMATTED-DATETIME("YYYYMMDD", datex) to RESULT
        move 8 to should_be
        if result not equal to should_be then
            display "TEST-FORMATTED-DATETIME("
                    """YYYYMMDD"""
                    ", "
                    function trim(datex)
                    ") should have been "
                    should_be " but was " result
            move 1 to return-code
            end-if.
        move "20000229" to datex
        move function TEST-FORMATTED-DATETIME("YYYYMMDD", datex) to RESULT
        move 0 to should_be
        if result not equal to should_be then
            display "TEST-FORMATTED-DATETIME("
                    """YYYYMMDD"""
                    ", "
                    function trim(datex)
                    ") should have been "
                    should_be " but was " result
            move 1 to return-code
            end-if.
        move "20007029" to datex
        move function TEST-FORMATTED-DATETIME("YYYYMMDD", datex) to RESULT
        move 5 to should_be
        if result not equal to should_be then
            display "TEST-FORMATTED-DATETIME("
                    """YYYYMMDD"""
                    ", "
                    function trim(datex)
                    ") should have been "
                    should_be " but was " result
            move 1 to return-code
            end-if.
      *>    TESTING YYYY-DDD
        move "1988" to yyyy of yyyydddv
        move "000"  to ddd  of yyyydddv
        move function TEST-FORMATTED-DATETIME("YYYY-DDD", yyyydddv) to RESULT
        move 8 to should_be
        if result not equal to should_be then
            display "TEST-FORMATTED-DATETIME("
                    """YYYYDDD"""
                    ", "
                    function trim(yyyydddv)
                    ") should have been "
                    should_be " but was " result
            move 1 to return-code
            end-if.
        move "1988" to yyyy of yyyydddv
        move "367"  to ddd  of yyyydddv
        move function TEST-FORMATTED-DATETIME("YYYY-DDD", yyyydddv) to RESULT
        move 8 to should_be
        if result not equal to should_be then
            display "TEST-FORMATTED-DATETIME("
                    """YYYYDDD"""
                    ", "
                    function trim(yyyydddv)
                    ") should have been "
                    should_be " but was " result
            move 1 to return-code
            end-if.
        move "1988" to yyyy of yyyydddv
        move "399"  to ddd  of yyyydddv
        move function TEST-FORMATTED-DATETIME("YYYY-DDD", yyyydddv) to RESULT
        move 7 to should_be
        if result not equal to should_be then
            display "TEST-FORMATTED-DATETIME("
                    """YYYYDDD"""
                    ", "
                    function trim(yyyydddv)
                    ") should have been "
                    should_be " but was " result
            move 1 to return-code
            end-if.
        move 1988 to yyyy of yyyydddv
        move 400  to ddd  of yyyydddv
        move function TEST-FORMATTED-DATETIME("YYYY-DDD", yyyydddv) to RESULT
        move 6 to should_be
        if result not equal to should_be then
            display "TEST-FORMATTED-DATETIME("
                    """YYYYDDD"""
                    ", "
                    function trim(yyyydddv)
                    ") should have been "
                    should_be " but was " result
            move 1 to return-code
            end-if.
        move 1988 to yyyy of yyyydddv
        perform varying i from 1 by 1 until i > 366
            move i to ddd of yyyydddv
            move function TEST-FORMATTED-DATETIME("YYYY-DDD", yyyydddv) to RESULT
            move zero to should_be
            if result not equal to should_be then
                display "TEST-FORMATTED-DATETIME("
                        """YYYY-DDD"""
                        ", "
                        function trim(yyyydddv)
                        ") should have been "
                        should_be " but was " result
                move 1 to return-code
                end-if
            add 1 to date-integer
            end-perform.
        end program test.

