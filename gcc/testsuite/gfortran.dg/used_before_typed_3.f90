! { dg-do compile }
! { dg-options "-std=f95" }

! PR fortran/32095
! PR fortran/34228
! Check for a special case when the return-type of a function is given outside
! its "body" and contains symbols defined inside.

MODULE testmod
  IMPLICIT REAL(a-z)

CONTAINS

  CHARACTER(len=x) FUNCTION test1 (x) ! { dg-error "of INTEGER" }
    IMPLICIT REAL(a-z)
    INTEGER :: x ! { dg-error "already has basic type" }
    test1 = "foobar"
  END FUNCTION test1

  CHARACTER(len=x) FUNCTION test2 (x) ! { dg-error "of INTEGER" }
    IMPLICIT INTEGER(a-z)
    test2 = "foobar"
  END FUNCTION test2

END MODULE testmod
  
CHARACTER(len=i) FUNCTION test3 (i)
  ! i is IMPLICIT INTEGER by default
  test3 = "foobar"
END FUNCTION test3

CHARACTER(len=g) FUNCTION test4 (g) ! { dg-error "of INTEGER" }
  ! g is REAL, unless declared INTEGER.
  test4 = "foobar"
END FUNCTION test4

! Test an empty function works, too.
INTEGER FUNCTION test5 ()
END FUNCTION test5
