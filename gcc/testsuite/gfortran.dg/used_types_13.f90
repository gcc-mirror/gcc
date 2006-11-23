! { dg-do compile }
! Tests the fix for PR29820, which was another problem with derived type
! association.  Not all siblings were being searched for identical types.
!
! Contributed by Harald Anlauf  <anlauf@gmx.de>
!
module geo
  type geodetic
     real :: h
  end type geodetic
end module geo
module gfcbug44
  implicit none
contains
subroutine point ( gp)
  use geo
  type(geodetic),  intent(out) :: gp
  type(geodetic) :: gpx(1)
  gp = gpx(1)
end subroutine point
subroutine plane ()
  use geo
  type(geodetic)  :: gp
  call point ( gp)
end subroutine plane
end module gfcbug44
! { dg-final { cleanup-modules "geo gfcbug44" } }

