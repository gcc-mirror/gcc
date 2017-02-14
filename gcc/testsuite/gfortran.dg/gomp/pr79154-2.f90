! PR fortran/79154
! { dg-do compile }

pure real function foo (a, b)
  real, intent(in) :: a, b
!$omp taskwait				! { dg-error "may not appear in PURE or ELEMENTAL" }
  foo = a + b
end function foo
pure function bar (a, b)
  real, intent(in) :: a(8), b(8)
  real :: bar(8)
  integer :: i
!$omp do simd				! { dg-error "may not appear in PURE or ELEMENTAL" }
  do i = 1, 8
    bar(i) = a(i) + b(i)
  end do
end function bar
pure function baz (a, b)
  real, intent(in) :: a(8), b(8)
  real :: baz(8)
  integer :: i
!$omp do				! { dg-error "may not appear in PURE or ELEMENTAL" }
  do i = 1, 8
    baz(i) = a(i) + b(i)
  end do
!$omp end do				! { dg-error "may not appear in PURE or ELEMENTAL" }
end function baz
pure real function baz2 (a, b)
  real, intent(in) :: a, b
!$omp target map(from:baz2)		! { dg-error "may not appear in PURE or ELEMENTAL" }
  baz2 = a + b
!$omp end target			! { dg-error "may not appear in PURE or ELEMENTAL" }
end function baz2
elemental real function fooe (a, b)
  real, intent(in) :: a, b
!$omp taskyield				! { dg-error "may not appear in PURE or ELEMENTAL" }
  fooe = a + b
end function fooe
elemental real function baze (a, b)
  real, intent(in) :: a, b
!$omp target map(from:baz)		! { dg-error "may not appear in PURE or ELEMENTAL" }
  baze = a + b
!$omp end target			! { dg-error "may not appear in PURE or ELEMENTAL" }
end function baze
