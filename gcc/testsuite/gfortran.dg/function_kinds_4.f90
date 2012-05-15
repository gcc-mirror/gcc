! { dg-do run }
! Tests the fix for PR34471 in which function KINDs that were
! USE associated would cause an error.
!
! This only needs to be run once.
! { dg-options "-O2" }
!
! Contributed by Tobias Burnus <burnus@gcc.gnu.org>
!
module m1
  integer, parameter :: i1 = 1, i2 = 2
end module m1

module m2
  integer, parameter :: i1 = 8
end module m2

integer(i1) function three()
  use m1, only: i2
  use m2                ! This provides the function kind
  three = i1
  if(three /= kind(three)) call abort()
end function three

! At one stage during the development of the patch, this started failing
! but was not tested in gfortran.dg.  */
real (kind(0d0)) function foo ()
  foo = real (kind (foo))
end function

program main
implicit none
 interface
    integer(8) function three()
    end function three
 end interface
 integer, parameter :: i1 = 4
 integer :: i
 real (kind(0d0)) foo
 i = one()
 i = two()
 if(three() /= 8) call abort()
 if (int(foo()) /= 8) call abort ()
contains
 integer(i1) function one()  ! Host associated kind
   if (kind(one) /= 4) call abort()
   one = 1
 end function one
 integer(i1) function two()  ! Use associated kind
   use m1, only: i2
   use m2
   if (kind(two) /= 8) call abort()
   two = 1
 end function two
end program main
