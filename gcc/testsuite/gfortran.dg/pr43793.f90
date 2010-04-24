! { dg-do compile }
!
! PR fortran/30073
! PR fortran/43793
!
! Original code by Joost VandeVondele 
! Reduced and corrected code by Steven G. Kargl
!
module fft_tools
  implicit none
  integer, parameter :: lp = 8
contains
  subroutine sparse_alltoall (rs, rq, rcount)
    complex(kind=lp), dimension(:, :), pointer :: rs, rq
    integer, dimension(:) :: rcount
    integer :: pos
    pos = 1
    if (rcount(pos) /= 0) then
       rq(1:rcount(pos),pos) = rs(1:rcount(pos),pos)
    end if
  end subroutine sparse_alltoall
end module fft_tools
! { dg-final { cleanup-modules "fft_tools" } }
