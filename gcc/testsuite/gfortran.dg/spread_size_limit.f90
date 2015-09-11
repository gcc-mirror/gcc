! { dg-do compile }
! { dg-options "-fdump-tree-original" }
!
! Test the fix for PR40472 in which simplify_spread had mo limit on the
! siz that it would try to expand to.
!
! Contributed by Philippe Marguinaud <philippe.marguinaud@meteo.fr>
!
REAL, DIMENSION(720,360)          :: ZLON_MASK
ZLON_MASK(:,:)= SPREAD( (/ (JLON , JLON=1,720) /) , DIM=2, NCOPIES=360 )
print *, zlon_mask(100,100)
END
! { dg-final { scan-tree-dump-times "_gfortran_spread" 1 "original" } }

