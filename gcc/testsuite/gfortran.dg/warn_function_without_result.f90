! { dg-do compile }
! { dg-options "-Wreturn-type" }
!
! PR fortran/31463 - inconsistent warnings if function return value is not set
! PR fortran/33950 - Warning missing for function result not set
! PR fortran/34296 - Intent(out) and character functions with RESULT: Value-not-set warning
!
FUNCTION f1()            ! { dg-warning "not set" }
REAL :: f1
END FUNCTION

FUNCTION f2()            ! { dg-warning "not set" }
REAL, DIMENSION(1) :: f2
END FUNCTION

FUNCTION f3()            ! { dg-warning "not set" }
REAL, POINTER :: f3
END FUNCTION

FUNCTION f4()            ! { dg-warning "not set" }
REAL, DIMENSION(:), POINTER :: f4
END FUNCTION

FUNCTION f5()            ! { dg-warning "not set" }
REAL, DIMENSION(:), ALLOCATABLE :: f5
END FUNCTION

FUNCTION f6()            ! { dg-warning "not set" }
CHARACTER(2) :: f6
END FUNCTION



FUNCTION g1() RESULT(h)  ! { dg-warning "not set" }
REAL :: h
END FUNCTION

FUNCTION g2() RESULT(h)  ! { dg-warning "not set" }
REAL, DIMENSION(1) :: h
END FUNCTION

FUNCTION g3() RESULT(h)  ! { dg-warning "not set" }
REAL, POINTER :: h
END FUNCTION

FUNCTION g4() RESULT(h)  ! { dg-warning "not set" }
REAL, DIMENSION(:), POINTER :: h
END FUNCTION

FUNCTION g5() RESULT(h)  ! { dg-warning "not set" }
REAL, DIMENSION(:), ALLOCATABLE :: h
END FUNCTION

FUNCTION g6() RESULT(h)  ! { dg-warning "not set" }
CHARACTER(2) :: h
END FUNCTION

