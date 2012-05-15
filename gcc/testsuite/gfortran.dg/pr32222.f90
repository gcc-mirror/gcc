!PR fortran/32222
! { dg-do compile }

module splinemod
implicit none
integer, parameter :: dl = KIND(1.d0)
Type lSamples
  integer l(10)
end Type lSamples
end module splinemod

subroutine InterpolateClArr(lSet)
use splinemod
type (lSamples), intent(in) :: lSet
real(dl) xl(10)
xl = real(lSet%l,dl)
end subroutine InterpolateClArr
