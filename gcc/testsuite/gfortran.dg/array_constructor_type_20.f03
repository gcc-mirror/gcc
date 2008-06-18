! { dg-do compile }
! { dg-options "-std=f2003" }

! PR fortran/36517
! Check for incorrect error message with -std=f2003.
! This is the original test from PR 36517.

CHARACTER (len=*) MY_STRING(1:3)
PARAMETER ( MY_STRING = (/ CHARACTER (len=3) :: "AC", "B", "C" /) )
CHARACTER (len=*), PARAMETER :: str(2) = [ CHARACTER (len=3) :: 'A', 'cc' ]
END
