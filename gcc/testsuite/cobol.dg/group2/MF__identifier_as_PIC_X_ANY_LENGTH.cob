      *> Do not edit this generated file.  See README.txt
      *> { dg-do run }
       *> { dg-options "-dialect mf" }
[
        identification division.
        program-id. foo prototype.
        data division.
        linkage section.
        01 buf pic x any length.
        procedure division using buf.
        end program foo.

        identification division.
        program-id. prog.
        data division.
        working-storage section.
        77 buf pic x(64) value "hello".
        procedure division.
          call "foo" using buf.
        end program prog.

        identification division.
        program-id. foo.
        data division.
        linkage section.
        01 buf pic x any length.
        procedure division using buf.
          display "buf is " buf.
        end program foo.
]
