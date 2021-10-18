! { dg-do compile}
! { dg-additional-options "-fcoarray=single" }
!
! TS 29113
! C535a  An assumed-rank entity shall be a dummy variable that does not
! have the CODIMENSION or VALUE attribute.
! An assumed-rank object may have the CONTIGUOUS attribute.
!

! This test file contains tests that are expected to issue diagnostics
! for invalid code.

! Check that diagnostics are issued when dimension(..) is used to declare
! things that are not dummy variables.

subroutine s0 (a)
  implicit none
  integer :: a

  integer :: goodlocal
  integer :: badlocal1(..)  ! { dg-error "Assumed.rank" }
  integer, dimension(..) :: badlocal2  ! { dg-error "Assumed.rank" }
  integer :: badlocal3  ! { dg-error "Assumed.rank" }
  dimension badlocal3(..)

  integer :: goodcommon
  integer :: badcommon1(..)  ! { dg-error "Assumed.rank" }
  integer, dimension(..) :: badcommon2  ! { dg-error "Assumed.rank" }
  integer :: badcommon3  ! { dg-error "Assumed.rank" }
  dimension badcommon3(..)
  common /frob/ goodcommon, badcommon1, badcommon2, badcommon3

  integer :: goodstatic
  integer :: badstatic1(..)  ! { dg-error "Assumed.rank" }
  integer, dimension(..) :: badstatic2  ! { dg-error "Assumed.rank" }
  integer :: badstatic3  ! { dg-error "Assumed.rank" }
  dimension badstatic3(..)
  save goodstatic, badstatic1, badstatic2, badstatic3

  block
    integer :: goodblocklocal
    integer :: badblocklocal1(..)  ! { dg-error "Assumed.rank" }
    integer, dimension(..) :: badblocklocal2  ! { dg-error "Assumed.rank" }
    integer :: badblocklocal3  ! { dg-error "Assumed.rank" }
    dimension badblocklocal3(..)
  end block    

end subroutine

module m
  integer :: goodmodvar
  integer :: badmodvar1(..)  ! { dg-error "Assumed.rank" }
  integer, dimension(..) :: badmodvar2  ! { dg-error "Assumed.rank" }
  integer :: badmodvar3  ! { dg-error "Assumed.rank" }
  dimension badmodvar3(..)

  save goodmodvar, badmodvar1, badmodvar2, badmodvar3

  type :: t
    integer :: goodcomponent
    integer :: badcomponent1(..)  ! { dg-error "must have an explicit shape" }
    integer, dimension(..) :: badcomponent2  ! { dg-error "must have an explicit shape" }
  end type
end module
  
! Check that diagnostics are issued when dimension(..) is used in combination
! with the forbidden attributes.

subroutine s2 (b) ! { dg-error "has no IMPLICIT type" }
  implicit none
  integer, codimension[*] :: b(..) ! { dg-error "assumed-rank array" }
end subroutine

subroutine s5 (e) ! { dg-error "has no IMPLICIT type" }
  implicit none
  integer, value :: e(..) ! { dg-error "VALUE attribute conflicts with DIMENSION" }
end subroutine

