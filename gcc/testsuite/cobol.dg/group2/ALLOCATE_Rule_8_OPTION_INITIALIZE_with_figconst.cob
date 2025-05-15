       *> { dg-do run }
       *> { dg-output-file "group2/ALLOCATE_Rule_8_OPTION_INITIALIZE_with_figconst.out" }

        identification          division.
        program-id.             prog.
        procedure               division.
        display "initialize zeroes"
            call                    "prog-zeroes"
        display "initialize low-value"
            call                    "prog-low"
        display "initialize spaces"
            call                    "prog-space"
        display "initialize high-value"
            call                    "prog-high"
        continue.
        end program             prog.

        identification          division.
        program-id.             prog-space.
        options. initialize working-storage spaces.
        data                    division.
        working-storage         section.
        01   based-var          based.
         02  based-x            pic x(24) value "I am I, Don Quixote".
         02  based-9            pic 999   value 123.
         02  based-p            pointer   value NULL.
        01   allocated-pointer  pointer.
        procedure division.
        display     "allocate characters  (ISO 2023 Rule 8: OPT_INIT if specified, otherwise defaultbyte, otherwise zero)"
        allocate 35 characters returning allocated-pointer
        set address of based-var to allocated-pointer
        call        "reporter" using based-var
        free        allocated-pointer
        goback.
        end program             prog-space.

        identification          division.
        program-id.             prog-low.
        options. initialize working-storage low-values.
        data                    division.
        working-storage         section.
        01   based-var          based.
         02  based-x            pic x(24) value "I am I, Don Quixote".
         02  based-9            pic 999   value 123.
         02  based-p            pointer   value NULL.
        01   allocated-pointer  pointer.
        procedure division.
        display     "allocate characters  (ISO 2023 Rule 8: OPT_INIT if specified, otherwise defaultbyte, otherwise zero)"
        allocate 35 characters returning allocated-pointer
        set address of based-var to allocated-pointer
        call        "reporter" using based-var
        free        allocated-pointer
        goback.
        end program             prog-low.

        identification          division.
        program-id.             prog-zeroes.
        options. initialize working-storage binary zeroes.
        data                    division.
        working-storage         section.
        01   based-var          based.
         02  based-x            pic x(24) value "I am I, Don Quixote".
         02  based-9            pic 999   value 123.
         02  based-p            pointer   value NULL.
        01   allocated-pointer  pointer.
        procedure division.
        display     "allocate characters  (ISO 2023 Rule 8: OPT_INIT if specified, otherwise defaultbyte, otherwise zero)"
        allocate 35 characters returning allocated-pointer
        set address of based-var to allocated-pointer
        call        "reporter" using based-var
        free        allocated-pointer
        goback.
        end program             prog-zeroes.

        identification          division.
        program-id.             prog-high.
        options. initialize working-storage high-values.
        data                    division.
        working-storage         section.
        01   based-var          based.
         02  based-x            pic x(24) value "I am I, Don Quixote".
         02  based-9            pic 999   value 123.
         02  based-p            pointer   value NULL.
        01 pval redefines based-var pointer.
        01   allocated-pointer  pointer.
        procedure division.
        display     "allocate characters  (ISO 2023 Rule 8: OPT_INIT if specified, otherwise defaultbyte, otherwise zero)"
        allocate 35 characters returning allocated-pointer
        set address of based-var to allocated-pointer
        display pval
        free        allocated-pointer
        goback.
        end program             prog-high.

        identification          division.
        program-id.             reporter.
        data                    division.
        linkage                 section.
        01   based-var          based.
         02  based-x            pic x(24).
         02  based-9            pic 999  .
         02  based-p            pointer  .
        procedure division      using based-var.
        reportt.
            display "   (1) as allocated"
            perform reportt2
            goback.
        reportt2.
            display "       " """" based-x """" with no advancing
            display space     """" based-9 """" with no advancing
            display space       based-p.
            continue.
        end program             reporter.

