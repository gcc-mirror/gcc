! REPEAT intrinsic -- various checks should be enforced
!
! { dg-do compile }
program test
  use iso_c_binding, only: k => c_size_t
  implicit none
  character(len=0), parameter :: s0 = "" 
  character(len=1), parameter :: s1 = "a"
  character(len=2), parameter :: s2 = "ab"
  character(len=0) :: t0
  character(len=1) :: t1
  character(len=2) :: t2

  t0 = "" ; t1 = "a" ; t2 = "ab"

  ! Check for negative NCOPIES argument
  print *, repeat(s0, -1) ! { dg-error "Argument NCOPIES of REPEAT intrinsic is negative" }
  print *, repeat(s1, -1) ! { dg-error "Argument NCOPIES of REPEAT intrinsic is negative" }
  print *, repeat(s2, -1) ! { dg-error "Argument NCOPIES of REPEAT intrinsic is negative" }
  print *, repeat(t0, -1) ! { dg-error "Argument NCOPIES of REPEAT intrinsic is negative" }
  print *, repeat(t1, -1) ! { dg-error "Argument NCOPIES of REPEAT intrinsic is negative" }
  print *, repeat(t2, -1) ! { dg-error "Argument NCOPIES of REPEAT intrinsic is negative" }

  ! Check for too large NCOPIES argument and limit cases
  print *, repeat(t0, huge(0_k))
  print *, repeat(t1, huge(0_k))
  print *, repeat(t2, huge(0_k)) ! { dg-error "Argument NCOPIES of REPEAT intrinsic is too large " }
  print *, repeat(s2, huge(0_k)) ! { dg-error "Argument NCOPIES of REPEAT intrinsic is too large " }

  print *, repeat(t0, huge(0_k)/2)
  print *, repeat(t1, huge(0_k)/2)
  print *, repeat(t2, huge(0_k)/2)

  print *, repeat(t0, huge(0_k)/2+1)
  print *, repeat(t1, huge(0_k)/2+1)
  print *, repeat(t2, huge(0_k)/2+1) ! { dg-error "Argument NCOPIES of REPEAT intrinsic is too large " }
  print *, repeat(s2, huge(0_k)/2+1) ! { dg-error "Argument NCOPIES of REPEAT intrinsic is too large " }

end program test
