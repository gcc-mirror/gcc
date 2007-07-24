! { dg-do compile }
!
! PR fortran/32867 - nested initialization expression not simplified
!
! Testcase contributed by H. J. Lu <hjl AT lucon DOT org>
!

MODULE Readdata_mod
IMPLICIT NONE
Private
Public Parser
  integer, parameter :: nkeywords = 2
character(80), PARAMETER, dimension(1:nkeywords) :: keywords = &
(/'PROBLEMSIZE                                  ',             &
  'NFTRANS_TD                                   '/)

CONTAINS
SUBROUTINE Parser(nx, ny, keyword)
integer, intent(inout) :: nx, ny
character(80), intent(inout) :: keyword

select case (keyword)
  case (trim(keywords(1))) ! PROBLEMSIZE
    nx = 1
  case (trim(keywords(2))) !'NFTRANS_TD'
    ny = 1
end select

END SUBROUTINE Parser
END MODULE Readdata_mod

! { dg-final { cleanup-modules "Readdata_mod" } }
