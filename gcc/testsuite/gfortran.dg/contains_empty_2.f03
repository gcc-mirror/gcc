! { dg-do compile }
! { dg-options "-std=f2008 -pedantic" }

program test
  print *, 'hello there'
contains
end program test

module truc
  integer, parameter :: answer = 42
contains
end module truc
