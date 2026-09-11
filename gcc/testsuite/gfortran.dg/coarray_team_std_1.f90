! { dg-do compile }
! { dg-options "-fcoarray=lib -std=f2008" }
!
! PR 126781
!
! The TEAM/TEAM_NUMBER argument of NUM_IMAGES was gated on Fortran 2008 and
! that of IMAGE_INDEX was not gated at all; both are Fortran 2018.

program coarray_team_std_1
  implicit none

  integer :: caf[*], r

  r = num_images () ! ok
  r = num_images (1) ! { dg-error "Fortran 2018: 'team' or 'team_number' argument" }
  r = image_index (caf, [1]) ! ok
  r = image_index (caf, [1], 1) ! { dg-error "Fortran 2018: 'team' or 'team_number' argument" }
end program coarray_team_std_1
