! { dg-do compile }
!
! Test the fix for the testcase in comment 23 of PR96320, which used to
! fail with the message: Variable ‘new_foo’ cannot appear in a variable
! definition context.
!
! Contributed by Damian Rouson  <damian@sourceryinstitute.org>
!
module foobar
  implicit none

  type foo
    integer bar
  end type

  interface
    pure module function create() result(new_foo)
      implicit none
      type(foo) new_foo
    end function
  end interface

contains
  module procedure create
    new_foo%bar = 1  ! Error here
  end procedure
end module

  use foobar
  print *, create ()
end
