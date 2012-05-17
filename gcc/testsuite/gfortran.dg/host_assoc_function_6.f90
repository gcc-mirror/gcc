! { dg-do compile }
! Tests the fix for PR38765 in which the use associated symbol
! 'fun' was confused with the contained function in 'mod_b'
! because the real name was being used instead of the 'use'
! name..
!
! Contributed by Paul Thomas  <pault@gcc.gnu.org>
! from a report by Marco Restelli.
!
module mod_a
  implicit none
  public :: fun
  private
contains
  pure function fun(x) result(mu)
    real, intent(in) :: x(:,:)
    real :: mu(2,2,size(x,2))
    mu = 2.0
  end function fun
end module mod_a

module mod_b
  use mod_a, only: &
  a_fun => fun
  implicit none
  private
contains
  pure function fun(x) result(mu)
    real, intent(in) :: x(:,:)
    real :: mu(2,2,size(x,2))
    mu = a_fun(x)
  end function fun
end module mod_b
