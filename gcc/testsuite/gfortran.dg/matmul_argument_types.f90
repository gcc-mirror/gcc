! { dg-do compile }
!
! PR fortran/36355
! Check MATMUL argument types:
!
!           numeric   logical   other
! numeric      1         2        3
! logical      2         1        3
! other        3         3        3
!
! where
!   1    ok
!   2    argument type mismatch
!   3    invalid argument types
!

  INTEGER :: a(2,2)
  LOGICAL :: b(2,2)
  CHARACTER :: c

  a = MATMUL(a, a)            ! ok
  a = MATMUL(a, b)            ! { dg-error "must match" }
  a = MATMUL(a, c)            ! { dg-error "must be numeric or LOGICAL" }

  b = MATMUL(b, a)            ! { dg-error "must match" }
  b = MATMUL(b, b)            ! ok
  b = MATMUL(b, c)            ! { dg-error "must be numeric or LOGICAL" }

  c = MATMUL(c, a)            ! { dg-error "must be numeric or LOGICAL" }
  c = MATMUL(c, b)            ! { dg-error "must be numeric or LOGICAL" }
  c = MATMUL(c, c)            ! { dg-error "must be numeric or LOGICAL" }
END
