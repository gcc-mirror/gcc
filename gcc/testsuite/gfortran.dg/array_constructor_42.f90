! { dg-do compile }
!
! PR fortran/54730
! A symbol 'a' was created while attempting to parse a typespec in the array
! constructor.  That (invalid) symbol was kept until translation stage
! where it was leading to an ICE.
!
! Original testcase from Paul Kapinos <kapinos@rz.rwth-aachen.de>
!

  subroutine s
    implicit none
    intrinsic :: real
    real :: vec(1:2)
    vec = (/ real(a = 1), 1. /)
  end subroutine s

  program main
    implicit none
    intrinsic :: real
    print *,(/ real(a = 1) /)
  end
