! { dg-do compile }
! { dg-options "-std=f2003 -pedantic" }
program test
  print *, 'hello there'
contains ! { dg-error "Fortran 2008: CONTAINS statement without" }
end program test

module truc
  integer, parameter :: answer = 42
contains ! { dg-error "Fortran 2008: CONTAINS statement without" }
end module truc
