! { dg-do compile }
!
! PR fortran/126234
! Test case derived from the PR example
module amod
  real, parameter :: a1 = 1.0
end module amod

module bmod
  real, parameter :: a1 = 2.0
end module bmod

module cmod
  use amod
end module cmod

module mmod
  use amod
  use bmod
contains
  subroutine sub (x)
    use cmod
    real :: x
    x = 1.0
  end subroutine sub
end module mmod
