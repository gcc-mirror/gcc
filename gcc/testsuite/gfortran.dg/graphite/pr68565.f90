! { dg-do run }
! { dg-options "-floop-nest-optimize" }

MODULE test
  IMPLICIT NONE
  TYPE subset_type
     INTEGER                                    :: ncon_tot
     REAL(KIND=8),DIMENSION(:,:),ALLOCATABLE   :: coeff
  END TYPE
CONTAINS
  SUBROUTINE foo(subset)
      TYPE(subset_type)                        :: subset
      INTEGER :: icon1
      DO icon1=1,subset%ncon_tot
       subset%coeff(:,icon1)=subset%coeff(:,icon1)/&
        SQRT(DOT_PRODUCT(subset%coeff(:,icon1),subset%coeff(:,icon1)))
      END DO
  END SUBROUTINE
END MODULE

USE test
    TYPE(subset_type)                        :: subset
    INTEGER, VOLATILE :: n1=7,n2=4
    ALLOCATE(subset%coeff(n1,n2))
    CALL RANDOM_NUMBER(subset%coeff)
    subset%coeff=subset%coeff-0.5
    subset%ncon_tot=n2
    CALL foo(subset)
    WRITE(6,*) MAXVAL(subset%coeff)
END
