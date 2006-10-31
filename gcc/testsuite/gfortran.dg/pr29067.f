      ! { dg-do compile }
      ! PR fortran/29067
      implicit none
      integer :: n, i
      character(len=16),parameter :: s = "", s2 = "1234567890123456"

      i = 0 ; n = 9
      print *, s(9:16)
      print *, s2(9:16)
      if (s(9:16) == "90123456") then
      endif
      if (i > 0) then
        write (i,*) n
        call foo(0)
      endif
      do i = 1, n
      end do
      end
