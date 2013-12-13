! { dg-do link }
! { dg-options "-g" }
! { dg-additional-sources namelist_83_2.f90 }
!
! Note: compilation would be sufficient, but "compile" cannot be combined
! with dg-additional-sources.
!
! PR fortran/59440
!
! Contributed by Harald Anlauf
!
! Was ICEing during DWARF generation.
!
! This is the first file - dg-additional-sources contains the second one
!

module mo_t_datum
  implicit none
  integer :: qbit_conv = 0
end module mo_t_datum

! { dg-final { cleanup-modules "gfcbug126" } }
