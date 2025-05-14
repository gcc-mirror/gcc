!{ dg-do compile }
!{ dg-additional-options "-std=f2008" }

! TEAM_NUMBER= in coindices has been introduced in F2015 standard, but that is not
! dedicatedly supported by GFortran.  Therefore check for F2018.
program pr98903
  integer :: a[*]

  a = 42

  a = a[1, team_number=-1] ! { dg-error "Fortran 2018: TEAM_NUMBER= not supported at" }
end program pr98903

