! { dg-do compile }
! { dg-options "-Wcompare-reals" }
program main
  real :: a
  complex :: c
  read (*,*) a
  read (*,*) c
  if (a .eq. 3.14) print *,"foo" ! { dg-warning "Equality comparison for REAL" }
  if (3.14 == a) print *,"foo" ! { dg-warning "Equality comparison for REAL" }
  if (a .eq. 3) print *,"foo" ! { dg-warning "Equality comparison for REAL" }
  if (3. == a) print *,"foo" ! { dg-warning "Equality comparison for REAL" }
  if (a .ne. 4.14) print *,"foo" ! { dg-warning "Inequality comparison for REAL" }
  if (4.14 /= a) print *,"foo" ! { dg-warning "Inequality comparison for REAL" }
  if (a .ne. 4) print *,"foo" ! { dg-warning "Inequality comparison for REAL" }
  if (4 /= a) print *,"foo" ! { dg-warning "Inequality comparison for REAL" }

  if (c .eq. (3.14, 2.11)) print *,"foo" ! { dg-warning "Equality comparison for COMPLEX" }
  if ((3.14, 2.11) == a) print *,"foo" ! { dg-warning "Equality comparison for COMPLEX" }
  if (c .ne. (3.14, 2.11)) print *,"foo" ! { dg-warning "Inequality comparison for COMPLEX" }
  if ((3.14, 2.11) /= a) print *,"foo" ! { dg-warning "Inequality comparison for COMPLEX" }
end program main
