! { dg-do compile }
! { dg-options "-std=f95" }
!
! PR fortran/53111
!
! Contributed by Jacob Middag, reduced by Janus Weil.
!

module a
  type :: my
    real :: x
  end type
end module

module b
  use a
end module

program test
  use a
  use b
end program
