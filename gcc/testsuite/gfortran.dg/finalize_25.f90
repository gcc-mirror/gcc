! { dg-do run }
!
! PR fortran/58880
! PR fortran/60495
!
! Contributed by Andrew Benson and Janus Weil
!

module gn
  implicit none
  type sl
     integer, allocatable, dimension(:) :: lv
   contains
     final :: sld
  end type
  type :: nde
     type(sl) :: r
  end type nde

  integer :: cnt = 0

contains

  subroutine sld(s)
    type(sl) :: s
    cnt = cnt + 1
    ! print *,'Finalize sl'
  end subroutine
  subroutine ndm(s)
    type(nde), intent(inout) :: s
    type(nde)                :: i
    i=s
  end subroutine ndm
end module

program main
  use gn
  type :: nde2
     type(sl) :: r
  end type nde2
  type(nde) :: x

  cnt = 0
  call ndm(x)
  if (cnt /= 2) call abort()

  cnt = 0
  call ndm2()
  if (cnt /= 3) call abort()
contains
  subroutine ndm2
    type(nde2) :: s,i
    i=s
  end subroutine ndm2
end program main
