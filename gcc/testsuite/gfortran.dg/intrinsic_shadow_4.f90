! { dg-do compile }
! { dg-options "-Wall" }
!
! PR fortran/54199
!
subroutine test()
contains
  real function fraction(x) ! { dg-warning "'fraction' declared at .1. may shadow the intrinsic of the same name.  In order to call the intrinsic, explicit INTRINSIC declarations may be required." }
    real :: x
    fraction = x
  end function fraction
end subroutine test
