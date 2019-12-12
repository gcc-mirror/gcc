! { dg-do run }
!
! Tests the fix for PR87277 - runtime segfault as indicated.
!
! Contributed by Andrew Baldwin on clf.
!
      MODULE INTS_TYPE_MODULE
        TYPE INTS_TYPE
          INTEGER, ALLOCATABLE :: INTS(:)
        END TYPE INTS_TYPE
      CONTAINS
        SUBROUTINE ALLOCATE_INTS_TYPE (IT_OBJ)
          CLASS (INTS_TYPE), POINTER, INTENT (OUT) :: IT_OBJ

          ALLOCATE (INTS_TYPE :: IT_OBJ)

          SELECT TYPE (IT_OBJ)
          TYPE IS (INTS_TYPE)
            CALL ALLOCATE_ARRAY (IT_OBJ%INTS) ! Sefaulted at runtime here.
            if (.not.allocated (IT_OBJ%INTS)) stop 1
            if (any (IT_OBJ%INTS .ne. [1,2,3,4])) stop 2
          END SELECT

          RETURN
        END SUBROUTINE ALLOCATE_INTS_TYPE

        SUBROUTINE ALLOCATE_ARRAY (ALLOC_ARR)
          INTEGER, ALLOCATABLE, INTENT (OUT) :: ALLOC_ARR(:)
          INTEGER :: I

          ALLOCATE (ALLOC_ARR(4))

          DO I = 1, SIZE(ALLOC_ARR)
            ALLOC_ARR(I) = I
          END DO

          RETURN
        END SUBROUTINE ALLOCATE_ARRAY
      END MODULE INTS_TYPE_MODULE

      PROGRAM MFE
        USE INTS_TYPE_MODULE
        IMPLICIT NONE

        CLASS (INTS_TYPE), POINTER :: IT_OBJ

        CALL ALLOCATE_INTS_TYPE (IT_OBJ)
      END PROGRAM MFE
