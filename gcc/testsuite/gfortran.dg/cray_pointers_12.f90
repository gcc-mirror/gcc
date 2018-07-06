! { dg-do compile }
! { dg-options "-fcray-pointer" }
!
! Test the fix for PR36497 in which there was no error for the second
! declaration of 'x'.
!
! Contributed by Tobias Burnus  <burnus@gcc.gnu.org>
!
module test
 integer(8) ipt
 integer z(2), x
 pointer (ipt, x)
end module

program bar
 use test   ! { dg-error "conflicts with symbol" }
 integer x  ! { dg-error "conflicts with symbol" }
 ipt = loc(z(1))
 x = 1
 ipt = loc(z(2))
 x = 3
 if (any (z .ne. [1,3])) stop 1
end
