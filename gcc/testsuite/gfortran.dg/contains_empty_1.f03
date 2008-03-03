! { dg-do compile }
! { dg-options "-std=f2003 -pedantic" }
program test
  print *, 'hello there'
contains
end program test ! { dg-error "Fortran 2008: CONTAINS statement without" }

module truc
  integer, parameter :: answer = 42
contains
end module truc ! { dg-error "Fortran 2008: CONTAINS statement without" }
