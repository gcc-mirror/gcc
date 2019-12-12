! { dg-do compile }
! PR 92017 - this used to cause an ICE due do a missing charlen.
! Original test case by Gerhard Steinmetz.

program p
  character(3), parameter :: a(4) = 'abc'
  integer, parameter :: b(1) = minloc(a)
  integer, parameter :: c = minloc(a, dim=1)
  integer, parameter :: bb(1) = maxloc(a)
  integer, parameter :: c2 = maxloc(a,dim=1)
end program p
 
