! { dg-compile }
!
! Testcases from PR32002.
!
PROGRAM test_pr32002

  CALL test_1()                       ! scalar/vector
  CALL test_2()                       ! vector/vector
  CALL test_3()                       ! matrix/vector
  CALL test_4()                       ! matrix/matrix

CONTAINS
  ELEMENTAL FUNCTION f(x)
    INTEGER, INTENT(in) :: x
    INTEGER :: f
    f = x
  END FUNCTION

  SUBROUTINE test_1()
    INTEGER :: a = 0, b(2) = 0
    a = f(b)                          ! { dg-error "Incompatible ranks" }
    b = f(a)                          ! ok, set all array elements to f(a)
  END SUBROUTINE

  SUBROUTINE test_2()
    INTEGER :: a(2) = 0, b(3) = 0
    a = f(b)                          ! { dg-error "different shape" }
    a = f(b(1:2))                     ! ok, slice, stride 1
    a = f(b(1:3:2))                   ! ok, slice, stride 2
  END SUBROUTINE

  SUBROUTINE test_3()
    INTEGER :: a(4) = 0, b(2,2) = 0
    a = f(b)                          ! { dg-error "Incompatible ranks" }
    a = f(RESHAPE(b, (/ 4 /)))        ! ok, same shape
  END SUBROUTINE

  SUBROUTINE test_4()
    INTEGER :: a(2,2) = 0, b(3,3) = 0
    a = f(b)                          ! { dg-error "different shape" }
    a = f(b(1:3, 1:2))                ! { dg-error "different shape" }
    a = f(b(1:3:2, 1:3:2))            ! ok, same shape
  END SUBROUTINE
END PROGRAM
