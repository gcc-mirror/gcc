! { dg-do compile }
! Test the patch for PR30202 in which the INTENT(OUT)
! caused an ICE.
!
! Contributed by Salvatore Filippone <sfilippone@uniroma2.it>
!
program class_scal_p
  implicit none
  type scal_p
    real, allocatable :: b(:)
  end type scal_p
  type(scal_p) :: pd
  call psb_geallv(pd%b)
contains
  subroutine psb_geallv(x)
    real, allocatable, intent(out) :: x(:)
  end subroutine psb_geallv
end program class_scal_p
