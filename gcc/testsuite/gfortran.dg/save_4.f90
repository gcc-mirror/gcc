! { dg-do compile }
! { dg-options "-std=f2003" }
!
! PR fortran/53597
!
MODULE somemodule
  IMPLICIT NONE
  TYPE sometype
    INTEGER :: i
    DOUBLE PRECISION, POINTER, DIMENSION(:,:) :: coef => NULL()
  END TYPE sometype
  TYPE(sometype) :: somevariable ! { dg-error "Fortran 2008: Implied SAVE for module variable 'somevariable' at .1., needed due to the default initialization" }
END MODULE somemodule
