! { dg-do compile }
! PR fortran/20869
! Note 12.11 "A name shall not appear in both an EXTERNAL and an
! INTRINSIC statement in the same scoping unit.
program u
  intrinsic :: nint
  external :: nint  ! { dg-error "EXTERNAL attribute conflicts with INTRINSIC attribute" }
end program u
