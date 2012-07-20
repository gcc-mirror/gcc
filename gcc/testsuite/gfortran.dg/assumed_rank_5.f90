! { dg-do compile }
! { dg-options "-std=f2008" }
!
! PR fortran/48820
!
!
subroutine foo(x)
  integer :: x(..)  ! { dg-error "TS 29113: Assumed-rank array" }
end subroutine foo
