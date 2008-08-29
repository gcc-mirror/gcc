MODULE  M1
  IMPLICIT NONE
  TYPE mmm
    COMPLEX(KIND=8), DIMENSION(:,:), POINTER :: data
  END TYPE mmm

CONTAINS

  SUBROUTINE S(ma,mb,mc)
      TYPE(mmm), POINTER :: ma,mb,mc
      COMPLEX(KIND=8), DIMENSION(:, :), &
        POINTER                                :: a, b, c
      INTEGER :: i,j
      a=>ma%data
      b=>mb%data
      c=>mc%data
      DO i=1,size(a,1)
      DO j=1,size(a,2)
         c(i,j)=a(i,j)*b(i,j)
      ENDDO
      ENDDO
  END SUBROUTINE

END MODULE M1
