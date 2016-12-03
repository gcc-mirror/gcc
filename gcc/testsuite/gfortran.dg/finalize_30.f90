! { dg-do compile }
! { dg-options "-Wsurprising" }
!
! PR 58175: [OOP] Incorrect warning message on scalar finalizer
!
! Contributed by Andrew Benson <abensonca@gmail.com>

module ct
  type :: a
   contains
     final :: aD
  end type
  type, extends(a) :: a1
  end type
contains
  subroutine aD(self)
    type(a), intent(inout) :: self
  end subroutine
end module

program test
  use ct
end
