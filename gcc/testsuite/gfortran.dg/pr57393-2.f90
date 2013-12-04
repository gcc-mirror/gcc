! PR middle-end/57393
! { dg-do compile }
! { dg-options "-g -O2" }

SUBROUTINE pr57393 ( a1, a2, a3, a4, a5, a6, a7 )
  COMPLEX(kind=8), DIMENSION(:), INTENT(IN) :: a1
  INTEGER, DIMENSION(:), INTENT(IN) :: a2, a3, a5, a6
  COMPLEX(kind=8), DIMENSION(:), INTENT(INOUT) :: a4
  a4(a6(1)+1:a6(1)+a5(1))=a1(a3(1)+1:a3(1)+a2(1))
END SUBROUTINE pr57393
