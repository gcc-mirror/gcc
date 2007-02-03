! { dg-do compile }
! Tests the fix for PR30514 in which the bounds on m would cause an
! error and the rest would cause the compiler to go into an infinite
! loop.
! Contributed by Tobias Burnus <burnus@gcc.gnu.org>
!
integer :: i(2:0), j(1:0), m(1:-1)
integer, parameter :: k(2:0) = 0, l(1:0) = 0
i = k
j = l
m = 5
end

