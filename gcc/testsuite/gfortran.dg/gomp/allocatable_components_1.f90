! { dg-do compile }
!
! PR fortran/32467
! Derived types with allocatable components
!

MODULE test_allocatable_components
  type :: t
    integer, allocatable :: a(:)
  end type

CONTAINS
  SUBROUTINE test_copyin()
    TYPE(t), SAVE :: a

    !$omp threadprivate(a)
    !$omp parallel copyin(a)        ! { dg-error "has ALLOCATABLE components" }
      ! do something
    !$omp end parallel
  END SUBROUTINE

  SUBROUTINE test_copyprivate()
    TYPE(t) :: a

    !$omp single                    ! { dg-error "has ALLOCATABLE components" }
      ! do something
    !$omp end single copyprivate (a)
  END SUBROUTINE

  SUBROUTINE test_firstprivate
    TYPE(t) :: a

    !$omp parallel firstprivate(a)  ! { dg-error "has ALLOCATABLE components" }
      ! do something
    !$omp end parallel
  END SUBROUTINE

  SUBROUTINE test_lastprivate
    TYPE(t) :: a
    INTEGER :: i

    !$omp parallel do lastprivate(a)  ! { dg-error "has ALLOCATABLE components" }
      DO i = 1, 1
      END DO
    !$omp end parallel do
  END SUBROUTINE

  SUBROUTINE test_reduction
    TYPE(t) :: a(10)
    INTEGER :: i

    !$omp parallel do reduction(+: a)   ! { dg-error "must be of numeric type" }
    DO i = 1, SIZE(a)
    END DO
    !$omp end parallel do
  END SUBROUTINE
END MODULE
