! { dg-do compile }
!
! Provided by Steve Kargl.

subroutine foo(n,m)
  integer, intent(in) :: n, m
  integer a(n)
  real b(n)
  equivalence(a,b)
  if (m /= 2) then
      a = 1
      print *, a(1)
  else
      b = 42.
      print *, b(1)
   end if
end subroutine 

subroutine bar(m)
  integer, intent(in) :: m
  integer x(8)
  real y(8)
  equivalence(x,y)
  if (m /= 2) then
      x = 1
      print *, x(1)
  else
      y = 42.
      print *, y(1)
   end if
end subroutine 

! { dg-error "Array '.' at .1. with non-constant bounds cannot be an EQUIVALENCE object" " " { target *-*-* } 9 }
