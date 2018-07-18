! { dg-do compile }
! { dg-options "-ffrontend-optimize" }
! PR fortran/80142 - the location on the expression of the
! unrolled any statement was not correctly set.
! Test case by Harald Anlauf.
MODULE gfcbug140
  implicit none
  integer ,parameter :: WV_NONE        =  1
  integer, parameter :: WV_CDV_4       =  23
  integer, parameter :: WV_CDV_8       =  24
  integer, parameter :: wv_CDV_list(2) = [ WV_CDV_4, WV_CDV_8 ]
  integer            :: basis          = WV_NONE
contains
  subroutine wave_1d (x)
    real, intent(inout) :: x(:,:)
    integer             :: oldbase
    oldbase = basis
    if (any (basis == wv_CDV_list(:))) then
    end if
    basis = oldbase
  end subroutine wave_1d
  !-
  subroutine mr_gp_mat (A)
    real, intent(inout) :: A (:,:)
    call wave_1d (A)
  end subroutine mr_gp_mat
end module gfcbug140
