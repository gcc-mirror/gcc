! { dg-do compile }
!
! PR fortran/58880
!
! Contributed by Andrew Benson
!

module gn
  type sl
     integer, allocatable, dimension(:) :: lv
   contains
     final :: sld
  end type sl
  type :: nde
     type(sl) :: r
  end type nde
contains
  subroutine ndm(s)
    type(nde), intent(inout) :: s
    type(nde)                :: i    
    i=s
  end subroutine ndm
  subroutine sld(s)
    implicit none
    type(sl), intent(inout) :: s
    return
  end subroutine sld
end module gn
