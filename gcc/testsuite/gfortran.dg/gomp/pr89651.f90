! PR fortran/89651
! { dg-do compile }
! { dg-additional-options "-Wuninitialized" }

program pr89651
  integer :: n
  real, allocatable :: t(:)
  n = 10
  allocate (t(n), source = 0.0)
!$omp parallel firstprivate(t)
  print *, sum (t) ! { dg-bogus "lbound' may be used uninitialized in this function" }
                   ! { dg-bogus "ubound' may be used uninitialized in this function" "" { target *-*-* } .-1 }
                   ! { dg-bogus "offset' may be used uninitialized in this function" "" { target *-*-* } .-2 }
!$omp end parallel
!$omp parallel private(t)
  t = 0.0
  print *, sum (t) ! { dg-bogus "lbound' may be used uninitialized in this function" }
                   ! { dg-bogus "ubound' may be used uninitialized in this function" "" { target *-*-* } .-1 }
                   ! { dg-bogus "offset' may be used uninitialized in this function" "" { target *-*-* } .-2 }
!$omp end parallel
end program pr89651
