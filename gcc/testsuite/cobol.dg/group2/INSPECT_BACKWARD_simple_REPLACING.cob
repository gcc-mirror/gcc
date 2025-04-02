       *> { dg-do run }
       *> { dg-options "-dialect mf" }
       *> { dg-output-file "group2/INSPECT_BACKWARD_simple_REPLACING.out" }

        program-id.         prog.
        data                division.
        working-storage     section.
        01 item pic x(64).
        procedure division.

        move "AbcAbcXAbcAbcAbcYAbcAbcAbcAbcZAbcAbcAbcAbcAbc" to item
        display function trim(item)
        inspect backward item replacing all "Abc" by "Qrs"
        display function trim(item)

        move "AbcAbcXAbcAbcAbcYAbcAbcAbcAbcZAbcAbcAbcAbcAbc" to item
        display function trim(item)
        inspect backward item replacing trailing "Abc" by "Qrs"
        display function trim(item)

        move "AbcAbcXAbcAbcAbcYAbcAbcAbcAbcZAbcAbcAbcAbcAbc" to item
        display function trim(item)
        inspect backward item replacing all "Abc" by "Qrs"
                after "Z" before "Y"
        display function trim(item)

        goback.
        end program prog.

