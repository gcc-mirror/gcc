       *> { dg-do run }
       *> { dg-output-file "group2/ALLOCATE___FREE_basic_default_versions.out" }

        program-id.         prog.
        data                division.
        working-storage     section.
        01 based-var        pic x(100) based.
        01 mem-pointer      pointer.
        01 mem-size         pic 999 value 100.
        01 counter          pic 99 value zero.
        procedure division.
        allocate 100        characters returning mem-pointer.
            if mem-pointer equal NULL
                display "allocate 100 should not be NULL (1)"
            else
                add 1 to counter.
        free mem-pointer
            if mem-pointer not equal NULL
                display "mem-pointer should be NULL again (1)"
            else
                add 1 to counter.

        allocate mem-size   characters returning mem-pointer.
            if mem-pointer equal null
                display "allocate mem-size should not be NULL (2)"
            else
                add 1 to counter.
        free mem-pointer
            if mem-pointer not equal null
                display "mem-pointer should be NULL again (2)"
            else
                add 1 to counter.

        allocate based-var
            if address of based-var equal NULL
                display "address of based-var should not be NULL (1)"
            else
                add 1 to counter
        free based-var
            if address of based-var not equal NULL
                display "address of based-var be NULL (1)"
            else
                add 1 to counter.

        allocate based-var
            if address of based-var equal NULL
                display "address of based-var should not be NULL (2)"
            else
                add 1 to counter.
        free address of based-var
            if address of based-var not equal NULL
                display "address of based-var be NULL (2)"
            else
                add 1 to counter.

        allocate based-var returning mem-pointer.
        if address of based-var equal NULL
            display "address of based-var should not be NULL (3)"
        else
            add 1 to counter.
        if mem-pointer equal NULL
            display "address of mem-pointer should not be NULL (3)"
        else
            add 1 to counter.
        if address of based-var not equal mem-pointer
            display "address of mem-pointer should be equal to mem-pointer (3)"
        else
            add 1 to counter.

        display "There were " counter " successful tests; should be 11."
        goback.
        end program prog.

