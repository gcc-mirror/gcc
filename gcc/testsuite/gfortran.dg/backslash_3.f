C { dg-do run { target fd_truncate } }
C { dg-options "-fbackslash" }
C PR fortran/30278
      program a
      character(len=1), parameter  :: c1 = char(8), c2 = char(92)
      character(len=35) str1, str2
      character(len=37) :: str4, str3

      open(10, status='scratch')
      write(10, 100)
      rewind(10)
      read(10,'(A34)') str1
      str2 = 'Does ' // c1 // 'ackslash result in ' // c1 // 'ackslash'
      if (str1 .ne. str2) call abort

      rewind(10)
      write (10, 200)
      rewind(10)
      read(10,'(A37)') str3
      str4 = 'Does ' //c2// 'backslash result in ' //c2// 'backslash'
      if (str3 .ne. str4) call abort

      stop
 100  format ('Does \backslash result in \backslash')
 200  format ('Does \\backslash result in \\backslash')
      end
