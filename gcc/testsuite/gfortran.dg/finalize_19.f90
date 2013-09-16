! { dg-do compile }
!
! PR fortran/58356
!
! Contributed by Andrew Benson
!
module ct
  type :: cfl
   contains
     final :: cfld
  end type cfl
  type, extends(cfl) :: cfde
   contains
  end type cfde
contains
  subroutine cfld(self)
    implicit none
    type(cfl), intent(inout) :: self
    return
  end subroutine cfld
end module ct
