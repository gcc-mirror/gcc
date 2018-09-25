! { dg-do run }
!
! Test the fix for PR86863, where the Type Bound Procedures were
! not flagged as subroutines thereby causing an error at the call
! statements.
!
! Contributed by Damian Rouson  <damian@sourceryinstitute.org>
!
module foo
  implicit none
  integer :: flag = 0
  type bar
  contains
    procedure, nopass :: foobar
    procedure, nopass :: barfoo
  end type
contains
  subroutine foobar
    flag = 1
  end subroutine
  subroutine barfoo
    flag = 0
  end subroutine
end module

module foobartoo
  implicit none
  interface
    module subroutine set(object)
      use foo
      implicit none
      type(bar) object
    end subroutine
    module subroutine unset(object)
      use foo
      implicit none
      type(bar) object
    end subroutine
  end interface
contains
  module procedure unset
    use foo, only : bar
    call object%barfoo
  end procedure
end module

submodule(foobartoo) subfoobar
contains
  module procedure set
    use foo, only : bar
    call object%foobar
  end procedure
end submodule

  use foo
  use foobartoo
  type(bar) :: obj
  call set(obj)
  if (flag .ne. 1) stop 1
  call unset(obj)
  if (flag .ne. 0) stop 2
end
