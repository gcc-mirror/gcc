! { dg-do compile }
! { dg-additional-options "-Wall -Wno-return-type -Wno-unused-variable" }
!
! PR fortran/118796 - bogus recursion with DT default initialization

module m1
  implicit none

  type :: t1
     type(integer) :: f1 = 0
  end type t1

  TYPE :: c1
   contains
     procedure, public :: z
  END TYPE c1

contains
  ! type-bound procedure z has a default initialization
  function z( this )
    type(t1) :: z
    class(c1), intent(in) :: this
  end function z
end module m1

module m2
  use m1, only : c1
contains
  function z() result(field)
  end function z
end module m2

module m3
  use m1, only : c1
contains
  function z()
  end function z
end module m3
