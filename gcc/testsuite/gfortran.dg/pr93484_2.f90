! { dg-do compile }
!
program p
  implicit none
  integer, parameter :: x(4) = [1,2,3,4]
  print *, [real(x(k))] ! { dg-error "Symbol 'k' at .1. has no IMPLICIT type" }
end

