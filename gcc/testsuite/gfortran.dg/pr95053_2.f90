! { dg-do compile }
! PR 95053 - make sure we do not regress on 521.wrf_r from spec2017
!
function f (x)
  real, parameter :: cldeps = 0.
  f = 0.
  if (cldeps > 0.) then
     f = floor (x/cldeps) * cldeps
  end if
end function f
