! { dg-do run }
!
! PR libgfortran/53051
!
! Check that reading "4.0q0" works, i.e. floating-point
! numbers which use "q" to indicate the exponential.
! (Which is a vendor extension.)
!
      character(len=20) :: str
      real :: r
      integer :: i

      r = 0
      str = '1.0q0'
      read(str, *, iostat=i) r
      if (r /= 1.0 .or. i /= 0) call abort()
      !print *, r
      end
