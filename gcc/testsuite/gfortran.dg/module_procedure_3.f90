! { dg-do run }
!
! PR fortran/94348
!
! Contributed by Damian Rouson

module foo_module
  implicit none

  interface
     module function foo() result(bar)
       implicit none
       integer bar
     end function
  end interface

contains
  module procedure foo
    bar = 5
  end procedure
end module

program main
  use foo_module
  implicit none
  if (foo() /= 5) stop 1
end program main
