! { dg-do compile }
subroutine test_atomic
    integer (kind = 4) :: a
    integer :: b
    real :: c, f
    double precision :: d
    integer, dimension (10) :: e
    a = 1
    b = 2
    c = 3
    d = 4
    e = 5
    f = 6
!$omp atomic
    a = a + 4
!$omp atomic
    b = 4 - b
!$omp atomic
    c = c * 2
!$omp atomic
    d = 2 / d
!$omp atomic
    e = 1 ! { dg-error "must set a scalar variable" }
!$omp atomic
    a = a ** 8 ! { dg-error "assignment operator must be" }
!$omp atomic
    b = b + 3 + b ! { dg-error "cannot reference" }
!$omp atomic
    c = c - f + 1 ! { dg-error "not mathematically equivalent to" }
!$omp atomic
    a = ishft (a, 1) ! { dg-error "assignment intrinsic must be" }
!$omp atomic
    c = min (c, 2.1, c) ! { dg-error "intrinsic arguments except one" }
!$omp atomic
    a = max (b, e(1)) ! { dg-error "intrinsic argument must be 'a'" }
!$omp atomic
    d = 12 ! { dg-error "assignment must have an operator" }
end subroutine test_atomic
