! { dg-do compile }
! { dg-require-effective-target lp64 }
! { dg-require-effective-target fortran_integer_16 }
program main
  implicit none
  integer(kind=16), parameter :: l1 = 2_16**64_16
  character (len=2_16**64_16+4_16), parameter :: s = "" ! { dg-error "too large" }
  character (len=2_16**64_8+4_16) :: ch ! { dg-error "too large" }
  character (len=l1 + 1_16) :: v ! { dg-error "too large" }
  character (len=int(huge(0_8),kind=16) + 1_16) :: z ! { dg-error "too large" }
  character (len=int(huge(0_8),kind=16) + 0_16) :: w

  print *, len(s)

end program main
