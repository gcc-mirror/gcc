! { dg-do compile }
!
! PR 45366: Problem with procedure pointer dummy in PURE function
!
! Contributed by Marco Restelli <mrestelli@gmail.com>

module m1
 implicit none
 abstract interface
  pure function i_f(x) result(y)
   real, intent(in) :: x
   real :: y
  end function i_f
 end interface
end module m1

module m2
 use m1, only: i_f
 implicit none
contains
 pure function i_g(x,p) result(y)
  real, intent(in) :: x
  procedure(i_f), pointer, intent(in) :: p
  real :: y
   y = p(x)
 end function i_g
end module m2
