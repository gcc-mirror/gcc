       *> { dg-do run }

        identification division.
        program-id. test.
        data division.
        working-storage section.
        01 datev     pic 99999999.
        01 should_be pic 9999.
        01 result    pic 9999.
        procedure division.
        move function test-day-yyyyddd(1945123) to result
        move zero to should_be
        if result not equal to should_be then
            display "test-day-yyyyddd(1945123) should have been "
                    should_be " but was " result
            move 1 to return-code
            end-if.
        move function test-day-yyyyddd(10000000) to result
        move 1 to should_be
        if result not equal to should_be then
            display "test-day-yyyyddd(100000000) should have been "
                    should_be " but was " result
            move 1 to return-code
            end-if.
        move 1601000 to datev
        move 2 to should_be
        move function test-day-yyyyddd(datev) to result
        if result not equal to should_be then
            display "test-day-yyyyddd(" datev ") should have been "
                    should_be " but was " result
            move 1 to return-code
            end-if.
        move 1601001 to datev
        move zero to should_be
        move function test-day-yyyyddd(datev) to result
        if result not equal to should_be then
            display "test-day-yyyyddd(" datev ") should have been "
                    should_be " but was " result
            move 1 to return-code
            end-if.
        move 1601364 to datev
        move zero to should_be
        move function test-day-yyyyddd(datev) to result
        if result not equal to should_be then
            display "test-day-yyyyddd(" datev ") should have been "
                    should_be " but was " result
            move 1 to return-code
            end-if.
        move 1601365 to datev
        move zero to should_be
        move function test-day-yyyyddd(datev) to result
        if result not equal to should_be then
            display "test-day-yyyyddd(" datev ") should have been "
                    should_be " but was " result
            move 1 to return-code
            end-if.
        move 1601366 to datev
        move 2 to should_be
        move function test-day-yyyyddd(datev) to result
        if result not equal to should_be then
            display "test-day-yyyyddd(" datev ") should have been "
                    should_be " but was " result
            move 1 to return-code
            end-if.
        move 1601367 to datev
        move 2 to should_be
        move function test-day-yyyyddd(datev) to result
        if result not equal to should_be then
            display "test-day-yyyyddd(" datev ") should have been "
                    should_be " but was " result
            move 1 to return-code
            end-if.
        move 2000365 to datev
        move zero to should_be
        move function test-day-yyyyddd(datev) to result
        if result not equal to should_be then
            display "test-day-yyyyddd(" datev ") should have been "
                    should_be " but was " result
            move 1 to return-code
            end-if.
        move 2000366 to datev
        move zero to should_be
        move function test-day-yyyyddd(datev) to result
        if result not equal to should_be then
            display "test-day-yyyyddd(" datev ") should have been "
                    should_be " but was " result
            move 1 to return-code
            end-if.
        move 2000367 to datev
        move 2 to should_be
        move function test-day-yyyyddd(datev) to result
        if result not equal to should_be then
            display "test-day-yyyyddd(" datev ") should have been "
                    should_be " but was " result
            move 1 to return-code
            end-if.
        move 2100365 to datev
        move zero to should_be
        move function test-day-yyyyddd(datev) to result
        if result not equal to should_be then
            display "test-day-yyyyddd(" datev ") should have been "
                    should_be " but was " result
            move 1 to return-code
            end-if.
        move 2100366 to datev
        move 2 to should_be
        move function test-day-yyyyddd(datev) to result
        if result not equal to should_be then
            display "test-day-yyyyddd(" datev ") should have been "
                    should_be " but was " result
            move 1 to return-code
            end-if.
        move 2100367 to datev
        move 2 to should_be
        move function test-day-yyyyddd(datev) to result
        if result not equal to should_be then
            display "test-day-yyyyddd(" datev ") should have been "
                    should_be " but was " result
            move 1 to return-code
            end-if.
        move 1988365 to datev
        move zero to should_be
        move function test-day-yyyyddd(datev) to result
        if result not equal to should_be then
            display "test-day-yyyyddd(" datev ") should have been "
                    should_be " but was " result
            move 1 to return-code
            end-if.
        move 1988366 to datev
        move zero to should_be
        move function test-day-yyyyddd(datev) to result
        if result not equal to should_be then
            display "test-day-yyyyddd(" datev ") should have been "
                    should_be " but was " result
            move 1 to return-code
            end-if.
        move 1988367 to datev
        move 2 to should_be
        move function test-day-yyyyddd(datev) to result
        if result not equal to should_be then
            display "test-day-yyyyddd(" datev ") should have been "
                    should_be " but was " result
            move 1 to return-code
            end-if.
        move 1989365 to datev
        move zero to should_be
        move function test-day-yyyyddd(datev) to result
        if result not equal to should_be then
            display "test-day-yyyyddd(" datev ") should have been "
                    should_be " but was " result
            move 1 to return-code
            end-if.
        move 1989366 to datev
        move 2 to should_be
        move function test-day-yyyyddd(datev) to result
        if result not equal to should_be then
            display "test-day-yyyyddd(" datev ") should have been "
                    should_be " but was " result
            move 1 to return-code
            end-if.
        move 1989367 to datev
        move 2 to should_be
        move function test-day-yyyyddd(datev) to result
        if result not equal to should_be then
            display "test-day-yyyyddd(" datev ") should have been "
                    should_be " but was " result
            move 1 to return-code
            end-if.
        end program test.

