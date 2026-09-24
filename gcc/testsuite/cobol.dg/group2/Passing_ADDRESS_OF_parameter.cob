      *> Do not edit this generated file.  See README.txt
      *> { dg-do run }
       *> { dg-output-file "group2/Passing_ADDRESS_OF_parameter.out" }
        identification   division.
        function-id.     func.
        data             division.
        working-storage  section.
        01 foo pic 9999 based.
        linkage          section.
        01 pfoo pointer.
        01 zed pic 999 value zero.
        procedure division using by value pfoo returning zed.
           set address of foo to pfoo
           display "foo in func  " foo " should be ""1234"""
           goback.
        end function func.

        identification division.
        program-id. prog.
        data division.
        working-storage section.
        01 foo  pic 9999 value 1234.
        01 zed  pic 999.
        procedure division.
            move function func(address of foo) to zed
            call "prog2" using address of foo.
            goback.
        end program prog.

        identification division.
        program-id. prog2.
        data division.
        working-storage  section.
        01 foo pic 9999 based.
        linkage section.
        01 pfoo  pointer.
        procedure division using by value pfoo.
            set address of foo to pfoo
            display "foo in prog2 " foo " should be ""1234"""
            goback.
        end program prog2.

