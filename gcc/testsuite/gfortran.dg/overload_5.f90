! { dg-do run }
! PR fortran/109641
!
! Check overloading of intrinsic binary operators for numeric operands
! Reported by Adelson Oliveira

MODULE TESTEOP
  IMPLICIT NONE
  INTERFACE OPERATOR(.MULT.)
    MODULE PROCEDURE MULTr4
    MODULE PROCEDURE MULTc4
  END INTERFACE
  INTERFACE OPERATOR(*)
    MODULE PROCEDURE MULTr4
    MODULE PROCEDURE MULTc4
  END INTERFACE
  INTERFACE OPERATOR(==)
    MODULE PROCEDURE MULTr4
    MODULE PROCEDURE MULTc4
    MODULE PROCEDURE MULTr8
  END INTERFACE
  INTERFACE OPERATOR(<)
    MODULE PROCEDURE MULTc4
    MODULE PROCEDURE MULTi4
  END INTERFACE
  INTERFACE OPERATOR(**)
    MODULE PROCEDURE MULTc4
    MODULE PROCEDURE MULTi4
  END INTERFACE
  interface copy
     MODULE PROCEDURE copy
  end interface copy
CONTAINS
  elemental function copy (z)
    complex, intent(in) :: z
    complex             :: copy
    copy = z
  end function copy
  FUNCTION MULTr4(v,m)
    REAL,    INTENT(IN) :: v(:)
    REAL,    INTENT(IN) :: m(:,:)
    REAL                :: MULTr4(SIZE(m,DIM=1),SIZE(m,DIM=2))
    INTEGER             :: i
    FORALL(i=1:SIZE(v)) MULTr4(:,i)=m(:,i)*v(i)
  END FUNCTION MULTr4
  FUNCTION MULTr8(v,m)
    REAL,             INTENT(IN) :: v(:)
    double precision, INTENT(IN) :: m(:,:)
    double precision             :: MULTr8(SIZE(m,DIM=1),SIZE(m,DIM=2))
    INTEGER             :: i
    FORALL(i=1:SIZE(v)) MULTr8(:,i)=m(:,i)*v(i)
  END FUNCTION MULTr8
  FUNCTION MULTc4(v,m)
    REAL,    INTENT(IN) :: v(:)
    COMPLEX, INTENT(IN) :: m(:,:)
    COMPLEX             :: MULTc4(SIZE(m,DIM=1),SIZE(m,DIM=2))
    INTEGER             :: i
    FORALL(i=1:SIZE(v)) MULTc4(:,i)=m(:,i)*v(i)
  END FUNCTION MULTc4
  FUNCTION MULTi4(v,m)
    REAL,    INTENT(IN) :: v(:)
    integer, INTENT(IN) :: m(:,:)
    REAL                :: MULTi4(SIZE(m,DIM=1),SIZE(m,DIM=2))
    INTEGER             :: i
    FORALL(i=1:SIZE(v)) MULTi4(:,i)=m(:,i)*v(i)
  END FUNCTION MULTi4
END MODULE TESTEOP
PROGRAM TESTE
  USE TESTEOP
  implicit none
  type t
     complex :: c(3,3)
  end type t
  real,    parameter   :: vv(3)   = 42.
  complex, parameter   :: zz(3,3) = (1.0,0.0)
  integer, parameter   :: kk(3,3) = 2
  double precision     :: dd(3,3) = 3.d0
  COMPLEX, ALLOCATABLE :: m(:,:),r(:,:), s(:,:)
  REAL,    ALLOCATABLE :: v(:)
  type(t)              :: z(1) = t(zz)
  ALLOCATE(v(3),m(3,3),r(3,3),s(3,3))
  v = vv
  m = zz
  ! Original bug report
  r=v.MULT.m ! Reference
  s=v*m
  if (any (r /= s)) stop 1
  if (.not. all (r == s)) stop 2
  ! Check other binary intrinsics
  s=v==m
  if (any (r /= s)) stop 3
  s=v==copy(m)
  if (any (r /= s)) stop 4
  s=v==zz
  if (any (r /= s)) stop 5
  s=v==copy(zz)
  if (any (r /= s)) stop 6
  s=vv==m
  if (any (r /= s)) stop 7
  s=vv==copy(m)
  if (any (r /= s)) stop 8
  s=vv==zz
  if (any (r /= s)) stop 9
  s=vv==copy(zz)
  if (any (r /= s)) stop 10
  ! check if .eq. same operator as == etc.
  s=v.eq.m
  if (any (r /= s)) stop 11
  s=v.lt.z(1)%c
  if (any (r /= s)) stop 12
  s=v<((z(1)%c))
  if (any (r /= s)) stop 13
  if (.not. all (     1.   < (vv**kk))) stop 14
  if (.not. all (     1.   < (vv< kk))) stop 15
  if (.not. all ((42.,0.) == (v < m ))) stop 16
  if (.not. all ((42.,0.) == (v** m ))) stop 17
  if (.not. all ( 126.d0  == (vv==dd))) stop 18
END PROGRAM TESTE
