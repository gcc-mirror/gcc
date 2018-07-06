! { dg-do run }
!
! Tests the fix for PR68846 in which compiler generated temporaries were
! receiving the attributes of dummy arguments. This test is the original.
! The simplified versions by Gerhard Steinmetz are gratefully acknowledged.
!
! Contributed by Mirco Valentini  <mirco.valentini@polimi.it>
!
MODULE grid
  IMPLICIT NONE
  PRIVATE
  REAL(KIND=8), DIMENSION(100,100), TARGET :: WORKSPACE
  TYPE, PUBLIC :: grid_t
    REAL(KIND=8), DIMENSION(:,:), POINTER :: P => NULL ()
  END TYPE
  PUBLIC :: INIT
CONTAINS
  SUBROUTINE INIT (DAT)
    IMPLICIT NONE
    TYPE(grid_t), INTENT(INOUT) :: DAT
    INTEGER :: I, J
    DAT%P => WORKSPACE
    DO I = 1, 100
      DO J = 1, 100
        DAT%P(I,J) = REAL ((I-1)*100+J-1)
      END DO
    ENDDO
  END SUBROUTINE INIT
END MODULE grid

MODULE subgrid
  USE :: grid, ONLY: grid_t
  IMPLICIT NONE
  PRIVATE
  TYPE, PUBLIC :: subgrid_t
    INTEGER, DIMENSION(4) :: range
    CLASS(grid_t), POINTER    :: grd => NULL ()
  CONTAINS
    PROCEDURE, PASS :: INIT => LVALUE_INIT
    PROCEDURE, PASS :: JMP => LVALUE_JMP
  END TYPE
CONTAINS
  SUBROUTINE LVALUE_INIT (HOBJ, P, D)
    IMPLICIT NONE
    CLASS(subgrid_t),      INTENT(INOUT) :: HOBJ
    TYPE(grid_t), POINTER, INTENT(INOUT) :: P
    INTEGER, DIMENSION(4), INTENT(IN)    :: D
    HOBJ%range = D
    HOBJ%grd => P
  END SUBROUTINE LVALUE_INIT

  FUNCTION LVALUE_JMP(HOBJ, I, J) RESULT(P)
    IMPLICIT NONE
    CLASS(subgrid_t), INTENT(INOUT) :: HOBJ
    INTEGER, INTENT(IN) :: I, J
    REAL(KIND=8), POINTER :: P
    P => HOBJ%grd%P(HOBJ%range(1)+I-1, HOBJ%range(3)+J-1)
  END FUNCTION LVALUE_JMP
END MODULE subgrid

MODULE geom
  IMPLICIT NONE
CONTAINS
  SUBROUTINE fillgeom_03( subgrid, value  )
    USE :: subgrid, ONLY: subgrid_t
    IMPLICIT NONE
    TYPE(subgrid_T), intent(inout) :: subgrid
    REAL(kind=8),    intent(in) :: value
    INTEGER :: I, J
    DO i = 1, 3
      DO J = 1, 4
        subgrid%jmp(i,j) = value ! Dummy argument '_F.DA0' with INTENT(IN)
                                 ! in pointer association context or ICE
                                 ! in trans_decl.c, depending on INTENT of
                                 ! 'VALUE'
      ENDDO
    ENDDO
  END SUBROUTINE fillgeom_03
END MODULE geom

PROGRAM test_lvalue
  USE :: grid
  USE :: subgrid
  USE :: geom
  IMPLICIT NONE
  TYPE(grid_t), POINTER :: GRD => NULL()
  TYPE(subgrid_t) :: STENCIL
  REAL(KIND=8), POINTER :: real_tmp_ptr
  REAL(KIND=8), DIMENSION(10,10), TARGET :: AA
  REAL(KIND=8), DIMENSION(3,4) :: VAL
  INTEGER :: I, J, chksum
  integer, parameter :: r1 = 50
  integer, parameter :: r2 = 52
  integer, parameter :: r3 = 50
  integer, parameter :: r4 = 53
  DO I = 1, 3
    DO J = 1, 4
      VAL(I,J) = dble(I)*dble(J)
    ENDDO
  ENDDO

  ALLOCATE (GRD)
  CALL INIT (GRD)
  chksum = sum([([((i-1)*100 + j -1, j=1,100)], i = 1,100)])
  if (int(sum(grd%p)) .ne. chksum) stop 1

  CALL STENCIL%INIT (GRD, [r1, r2, r3, r4])
  if (.not.associated (stencil%grd, grd)) stop 2
  if (int(sum(grd%p)) .ne. chksum) stop 3

  CALL fillgeom_03(stencil, 42.0_8)
  if (any (int (grd%p(r1:r2,r3:r4)) .ne. 42)) stop 4

  chksum = chksum - sum([([((i - 1) * 100 + j -1, j=r3,r4)], i = r1,r2)]) &
           + (r4 - r3 + 1) * (r2 - r1 +1) * 42
  if (int(sum(grd%p)) .ne. chksum) stop 5

  deallocate (grd)
END PROGRAM test_lvalue


