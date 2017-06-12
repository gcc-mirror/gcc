! { dg-do run }
! { dg-options "-fcoarray=lib -lcaf_single -fdump-tree-original" }
! { dg-additional-options "-latomic" { target libatomic_available } }

program test_image_status_1
  use iso_fortran_env , only : STAT_STOPPED_IMAGE
  implicit none

  if (image_status(1) /= 0) error stop "image_status(1) should not fail"
  if (image_status(42) /= STAT_STOPPED_IMAGE) error stop "image_status(42) should report stopped image"

end program test_image_status_1

! { dg-final { scan-tree-dump-times "_gfortran_caf_image_status \\\(1, .+\\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "_gfortran_caf_image_status \\\(42, .+\\\)" 1 "original" } }


