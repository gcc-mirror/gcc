! { dg-do compile }
! { dg-options "-fdump-tree-original" }
!
! Test the fix for PR87284 in which the indexing in allocate with mold
! was incorrect for class array initialization and resulted in the valgrind
! error:
! "Conditional jump or move depends on uninitialised value(s)" at line 42.
!
! Contributed by Andrew Baldwin on clf.
!
      MODULE INTS_TYPE_MODULE
        TYPE, ABSTRACT :: BASE_TYPE
        END TYPE BASE_TYPE

        TYPE, EXTENDS (BASE_TYPE) :: INTS_TYPE
          INTEGER, ALLOCATABLE :: INTS(:)
        END TYPE INTS_TYPE
      CONTAINS
        SUBROUTINE MOLD_ALLOCATE (IT_OBJS, MOLD_OBJ)
          CLASS (BASE_TYPE), ALLOCATABLE, INTENT (OUT) :: IT_OBJS(:)
          CLASS (BASE_TYPE), INTENT (IN) :: MOLD_OBJ

          ALLOCATE (IT_OBJS(2), mold = MOLD_OBJ)

          RETURN
        END SUBROUTINE MOLD_ALLOCATE
      END MODULE INTS_TYPE_MODULE

      PROGRAM MFE
        USE INTS_TYPE_MODULE
        IMPLICIT NONE

        CLASS (BASE_TYPE), ALLOCATABLE :: IT_OBJS(:)
        INTEGER :: I
        TYPE (INTS_TYPE) :: MOLD_OBJ

        ALLOCATE (INTS_TYPE :: IT_OBJS(2))

        SELECT TYPE (IT_OBJS)
        TYPE IS (INTS_TYPE)
          ALLOCATE (IT_OBJS(1)%INTS(10))

          ALLOCATE (IT_OBJS(2)%INTS(10))
        END SELECT


        DEALLOCATE (IT_OBJS)

        CALL MOLD_ALLOCATE (IT_OBJS, MOLD_OBJ)

        IF (ALLOCATED(IT_OBJS)) THEN
          IF (SIZE(IT_OBJS) .GE. 2) THEN
            SELECT TYPE (IT_OBJS)
            TYPE IS (INTS_TYPE)
              ALLOCATE (IT_OBJS(1)%INTS(10))

              ALLOCATE (IT_OBJS(2)%INTS(10))
            END SELECT
          END IF
        END IF
      END PROGRAM MFE
! { dg-final { scan-tree-dump-times "it_objs->_vptr->_size" 1 "original" } }
