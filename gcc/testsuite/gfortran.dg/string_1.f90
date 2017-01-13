! { dg-do compile }
!
program main
  implicit none
  integer(kind=8), parameter :: l1 = 2_8**32_8
  character (len=2_8**32_8+4_8), parameter :: s = "" ! { dg-error "too large" }
  character (len=2_8**32_8+4_8) :: ch ! { dg-error "too large" }
  character (len=l1 + 1_8) :: v ! { dg-error "too large" }
  character (len=int(huge(0_4),kind=8) + 1_8) :: z ! { dg-error "too large" }
  character (len=int(huge(0_4),kind=8) + 0_8) :: w

  print *, len(s)

end program main
