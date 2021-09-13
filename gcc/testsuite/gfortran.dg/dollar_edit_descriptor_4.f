! { dg-do run }
! { dg-options "-std=gnu" }
! PR98825 Test for fix of '$' edit descriptor.
      character(30) :: line
   10 format (i3,$)

      open(10, status='scratch')
      write (10,10) 1
      write (10,10) 2,3,4,5
! Check the result.
      line = 'abcdefg'
      rewind(10)
      read(10, '(a)') line
      close(10)
      if (line .ne. '  1  2  3  4  5') call abort
      end
