      *> Do not edit this generated file.  See README.txt
      *> { dg-do run }
       *> { dg-output-file "group2/Read_and_write_errno.out" }
[
        copy "posix-errno.cpy".
        id division.
        program-id. prog.
        data division.
        working-storage section.
        01 msg pic x(64).
        77 my-errno binary-long.
        procedure division.
          move 2 to my-errno.
          move function posix-errno(msg, my-errno) to my-errno.
          move 0 to my-errno.
          move function posix-errno(msg) to my-errno.
          display "errno=" my-errno.

        end program prog.
]
