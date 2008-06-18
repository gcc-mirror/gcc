! { dg-do compile }
! { dg-options "-std=f2003" }

! PR fortran/36517
! Check for incorrect error message with -std=f2003.
! This is the test of comment #1, PR 36517.

print *, [ character(len=2) :: 'a', 'bb' ]
end
