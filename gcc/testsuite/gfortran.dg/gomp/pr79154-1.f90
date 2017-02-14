! PR fortran/79154
! { dg-do compile }

pure real function foo (a, b)
!$omp declare simd(foo)			! { dg-bogus "may not appear in PURE or ELEMENTAL" }
  real, intent(in) :: a, b
  foo = a + b
end function foo
pure function bar (a, b)
  real, intent(in) :: a(8), b(8)
  real :: bar(8)
  integer :: i
!$omp simd				! { dg-bogus "may not appear in PURE or ELEMENTAL" }
  do i = 1, 8
    bar(i) = a(i) + b(i)
  end do
end function bar
pure real function baz (a, b)
!$omp declare target			! { dg-bogus "may not appear in PURE or ELEMENTAL" }
  real, intent(in) :: a, b
  baz = a + b
end function baz
elemental real function fooe (a, b)
!$omp declare simd(fooe)		! { dg-bogus "may not appear in PURE or ELEMENTAL" }
  real, intent(in) :: a, b
  fooe = a + b
end function fooe
elemental real function baze (a, b)
!$omp declare target			! { dg-bogus "may not appear in PURE or ELEMENTAL" }
  real, intent(in) :: a, b
  baze = a + b
end function baze
