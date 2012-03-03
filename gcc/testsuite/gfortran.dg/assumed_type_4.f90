! { dg-do compile }
! { dg-options "-std=f2008" }
!
! PR fortran/48820
!
! Test TYPE(*)

subroutine one(a) ! { dg-error "TS 29113: Assumed type" }
  type(*)  :: a
end subroutine one
