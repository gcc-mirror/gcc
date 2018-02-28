! { dg-do run }
! PR31880 silent data corruption in gfortran read statement
! Test from PR.
      program r3

      integer(kind=4) :: a(1025),b(1025),c(1025),d(2048),e(1022)
      
      a = 5
      b = 6
      c = 7
      e = 8

      do i=1,2048
         d(i)=i
      end do

      open  (3,form='unformatted', status="scratch")
      write (3) a,b,c,d,e
      rewind 3
      d = 0
      read  (3) a,b,c,d
      close (3)

      if (d(1).ne.1) STOP 1
      if (d(2048).ne.2048) STOP 2

      end
