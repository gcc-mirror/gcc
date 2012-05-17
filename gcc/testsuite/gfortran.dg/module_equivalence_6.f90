! { dg-do compile }
!
! Fixes PR38171 a regression caused by the fix for PR37706.
!
! Contributed by Scot Breitenfeld <brtnfld@hdfgroup.org>
!
MODULE H5GLOBAL
  IMPLICIT NONE
  INTEGER :: H5P_flags
  INTEGER :: H5P_DEFAULT_F
  EQUIVALENCE(H5P_flags, H5P_DEFAULT_F)
END MODULE H5GLOBAL
MODULE HDF5
  USE H5GLOBAL
END MODULE HDF5
PROGRAM fortranlibtest
  USE HDF5
  IMPLICIT NONE
  INTEGER :: ii
  ii = H5P_DEFAULT_F 
END PROGRAM fortranlibtest
