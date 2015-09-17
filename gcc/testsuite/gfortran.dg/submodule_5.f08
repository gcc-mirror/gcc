! { dg-do compile }
!
! Checks that PRIVATE/PUBLIC not allowed in submodules. Also, IMPORT
! is not allowed in a module procedure interface body.
!
! Contributed by Paul Thomas  <pault@gcc.gnu.org>
!
module foo_interface
  implicit none
  type foo
    character(len=16), private :: byebye = "adieu, world!   "
  end type foo

! This interface is required to trigger the output of an .smod file.
! See http://j3-fortran.org/doc/meeting/207/15-209.txt
  interface
    integer module function trigger_smod ()
    end function
  end interface

end module

module foo_interface_brother
  use foo_interface
  implicit none
  interface
     module subroutine array3(this, that)
       import ! { dg-error "not permitted in a module procedure interface body" }
       type(foo), intent(in), dimension(:) :: this
       type(foo), intent(inOUT), allocatable, dimension(:) :: that
     end subroutine
  end interface
end module

SUBMODULE (foo_interface) foo_interface_son
  private ! { dg-error "PRIVATE statement" }
  public ! { dg-error "PUBLIC statement" }
  integer, public :: i ! { dg-error "PUBLIC attribute" }
  integer, private :: j ! { dg-error "PRIVATE attribute" }
  type :: bar
    private ! { dg-error "PRIVATE statement" }
    public ! { dg-error "PUBLIC statement" }
    integer, private :: i ! { dg-error "PRIVATE attribute" }
    integer, public :: j ! { dg-error "PUBLIC attribute" }
  end type bar
contains
!
end submodule foo_interface_son

SUBMODULE (foo_interface) foo_interface_daughter
!
contains
  subroutine foobar (arg)
    type(foo) :: arg
    arg%byebye = "hello, world!   " ! Access to private component is OK
  end subroutine
end SUBMODULE foo_interface_daughter

end
! { dg-final { cleanup-submodules "foo_interface@foo_interface_daughter" } }
