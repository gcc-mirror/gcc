! { dg-do "compile" }
! { dg-options "-c -Wall" }
!
! PR fortran/42360
!
MODULE m
  TYPE :: t1
    INTEGER :: a = 42, b
  END TYPE

  TYPE :: t2
    INTEGER :: a, b
  END TYPE

CONTAINS
  SUBROUTINE sub1(x)             ! no warning, default initializer
    type(t1), intent(out) :: x
  END SUBROUTINE

  SUBROUTINE sub2(x)             ! no warning, initialized
    type(t2), intent(out) :: x
    x%a = 42
  END SUBROUTINE

  SUBROUTINE sub3(x)             ! { dg-warning "not set" }
    type(t2), intent(out) :: x
  END SUBROUTINE
END MODULE

! { dg-final { cleanup-modules "m" } }
