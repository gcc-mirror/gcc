! { dg-do compile }
!
! PR fortran/50898
! A symbol was freed prematurely during resolution,
! despite remaining reachable
!
! Original testcase from <shaojuncycle@gmail.com>

MODULE MODULE_pmat2

IMPLICIT NONE

INTERFACE cad1b;  MODULE PROCEDURE cad1b;          END INTERFACE
INTERFACE csb1b;  MODULE PROCEDURE csb1b;          END INTERFACE
INTERFACE copbt;  MODULE PROCEDURE copbt;          END INTERFACE
INTERFACE conbt;  MODULE PROCEDURE conbt;          END INTERFACE
INTERFACE copmb;  MODULE PROCEDURE copmb;          END INTERFACE
INTERFACE conmb;  MODULE PROCEDURE conmb;          END INTERFACE
INTERFACE copbm;  MODULE PROCEDURE copbm;          END INTERFACE
INTERFACE conbm;  MODULE PROCEDURE conbm;          END INTERFACE
INTERFACE mulvb;  MODULE PROCEDURE mulvb;          END INTERFACE
INTERFACE madvb;  MODULE PROCEDURE madvb;          END INTERFACE
INTERFACE msbvb;  MODULE PROCEDURE msbvb;          END INTERFACE
INTERFACE mulxb;  MODULE PROCEDURE mulxb;          END INTERFACE
INTERFACE madxb;  MODULE PROCEDURE madxb;          END INTERFACE
INTERFACE msbxb;  MODULE PROCEDURE msbxb;          END INTERFACE

integer, parameter :: i_kind=4
integer, parameter :: r_kind=4
real(r_kind), parameter :: zero=0.0
real(r_kind), parameter :: one=1.0
real(r_kind), parameter :: two=2.0

CONTAINS

SUBROUTINE cad1b(a,m1,mah1,mah2,mirror2)
implicit none
INTEGER(i_kind),  INTENT(IN   ) :: m1,mah1,mah2,mirror2
REAL(r_kind),     INTENT(INOUT) :: a(0:m1-1,-mah1:mah2)
RETURN
ENTRY     csb1b(a,m1,mah1,mah2,mirror2)
END SUBROUTINE cad1b

SUBROUTINE copbt(a,b,m1,m2,mah1,mah2)
implicit none
INTEGER(i_kind),  INTENT(IN   ) :: m1, m2, mah1, mah2
REAL(r_kind),     INTENT(IN   ) :: a(m1,-mah1:mah2)
REAL(r_kind),     INTENT(  OUT) :: b(m2,-mah2:mah1)
RETURN
ENTRY    conbt(a,b,m1,m2,mah1,mah2)
END SUBROUTINE copbt

SUBROUTINE copmb(afull,aband,m1,m2,mah1,mah2)
implicit none
INTEGER(i_kind),                           INTENT(IN   ) :: m1, m2, mah1, mah2
REAL(r_kind),     DIMENSION(m1,m2),        INTENT(IN   ) :: afull
REAL(r_kind),     DIMENSION(m1,-mah1:mah2),INTENT(  OUT) :: aband
RETURN
ENTRY      conmb(afull,aband,m1,m2,mah1,mah2)
END SUBROUTINE copmb

SUBROUTINE copbm(aband,afull,m1,m2,mah1,mah2)
implicit none
INTEGER(i_kind),                           INTENT(IN   ) :: m1, m2, mah1, mah2
REAL(r_kind),     DIMENSION(m1,-mah1:mah2),INTENT(IN   ) :: aband
REAL(r_kind),     DIMENSION(m1,m2),        INTENT(  OUT) :: afull
RETURN
ENTRY      conbm(aband,afull,m1,m2,mah1,mah2)
END SUBROUTINE copbm

SUBROUTINE mulbb(a,b,c,m1,m2,mah1,mah2,mbh1,mbh2,mch1,mch2)
implicit none
INTEGER(i_kind),  INTENT(IN   ) :: m1, m2, mah1, mah2, mbh1, mbh2, mch1, mch2
REAL(r_kind),     INTENT(IN   ) :: a(m1,-mah1:mah2), b(m2,-mbh1:mbh2)
REAL(r_kind),     INTENT(INOUT) :: c(m1,-mch1:mch2)
INTEGER(i_kind)                :: nch1, nch2, j, k, jpk, i1,i2
c=zero
ENTRY      madbb(a,b,c,m1,m2,mah1,mah2,mbh1,mbh2,mch1,mch2)
nch1=mah1+mbh1; nch2=mah2+mbh2
IF(nch1 /= mch1 .OR. nch2 /= mch2)STOP 'In MULBB, dimensions inconsistent'
DO j=-mah1,mah2
   DO k=-mbh1,mbh2; jpk=j+k; i1=MAX(1,1-j); i2=MIN(m1,m2-j)
      c(i1:i2,jpk)=c(i1:i2,jpk)+a(i1:i2,j)*b(j+i1:j+i2,k)
   ENDDO
ENDDO
END SUBROUTINE mulbb

SUBROUTINE MULVB(v1,a,v2, m1,m2,mah1,mah2)
implicit none
INTEGER(i_kind),  INTENT(IN   ) :: m1, m2, mah1, mah2
REAL(r_kind),     INTENT(IN   ) :: v1(m1), a(m1,-mah1:mah2)
REAL(r_kind),     INTENT(  OUT) :: v2(m2)
INTEGER(i_kind)                 :: j, i1,i2
v2=zero
ENTRY    madvb(v1,a,v2, m1,m2,mah1,mah2)
DO j=-mah1,mah2; i1=MAX(1,1-j); i2=MIN(m1,m2-j)
   v2(j+i1:j+i2)=v2(j+i1:j+i2)+v1(i1:i2)*a(i1:i2,j)
ENDDO
RETURN
ENTRY    msbvb(v1,a,v2, m1,m2,mah1,mah2)
DO j=-mah1,mah2; i1=MAX(1,1-j); i2=MIN(m1,m2-j)
   v2(j+i1:j+i2)=v2(j+i1:j+i2)-v1(i1:i2)*a(i1:i2,j)
ENDDO
END SUBROUTINE mulvb

SUBROUTINE mulxb(v1,a,v2, m1,m2,mah1,mah2,my)
implicit none
INTEGER(i_kind),  INTENT(IN   ) :: m1, m2, mah1, mah2, my
REAL(r_kind),     INTENT(IN   ) :: v1(m1,my), a(m1,-mah1:mah2)
REAL(r_kind),     INTENT(  OUT) :: v2(m2,my)
INTEGER(i_kind)                 :: i,j
v2=zero
ENTRY    madxb(v1,a,v2, m1,m2,mah1,mah2,my)
DO j=-mah1,mah2
   DO i=MAX(1,1-j),MIN(m1,m2-j); v2(j+i,:)=v2(j+i,:)+v1(i,:)*a(i,j); ENDDO
ENDDO
RETURN
ENTRY    msbxb(v1,a,v2, m1,m2,mah1,mah2,my)
DO j=-mah1,mah2
   DO i=MAX(1,1-j),MIN(m1,m2-j); v2(j+i,:)=v2(j+i,:)-v1(i,:)*a(i,j); ENDDO
ENDDO
END SUBROUTINE mulxb

SUBROUTINE mulyb(v1,a,v2, m1,m2,mah1,mah2,mx)
implicit none
INTEGER(i_kind),  INTENT(IN   ) :: m1, m2, mah1, mah2, mx
REAL(r_kind),     INTENT(IN   ) :: v1(mx,m1), a(m1,-mah1:mah2)
REAL(r_kind),     INTENT(  OUT) :: v2(mx,m2)
INTEGER(i_kind)                 :: i,j
v2=zero
ENTRY    madyb(v1,a,v2, m1,m2,mah1,mah2,mx)
DO j=-mah1,mah2
    DO i=MAX(1,1-j),MIN(m1,m2-j)
      v2(:,j+i)=v2(:,j+i)+v1(:,i)*a(i,j)
    ENDDO
ENDDO
RETURN
ENTRY    msbyb(v1,a,v2, m1,m2,mah1,mah2,mx)
 DO j=-mah1,mah2
    DO i=MAX(1,1-j),MIN(m1,m2-j)
       v2(:,j+i)=v2(:,j+i)-v1(:,i)*a(i,j)
    ENDDO
 ENDDO
RETURN
END SUBROUTINE mulyb

END MODULE MODULE_pmat2

