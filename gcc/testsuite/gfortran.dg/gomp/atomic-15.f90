! { dg-do compile }
! { dg-options "-fopenmp" }
module m
  implicit none
  integer :: x = 6
end module m

program main
  use m
  implicit none
  integer v
  !$omp atomic
    x = x * 7 + 6       ! { dg-error "assignment must be var = var op expr or var = expr op var" }
  !$omp atomic
    x = ieor (x * 7, 6)       ! { dg-error "intrinsic arguments except one must not reference 'x'" }
  !$omp atomic update
    x = x - 8 + 6       ! { dg-error "var = var op expr not mathematically equivalent to var = var op \\(expr\\)" }
  !$omp atomic
    x = ior (ieor (x, 7), 2)       ! { dg-error "intrinsic arguments except one must not reference 'x'" }
  !$omp atomic
    x = x / 7 * 2       ! { dg-error "var = var op expr not mathematically equivalent to var = var op \\(expr\\)" }
  !$omp atomic
    x = x / 7 / 2       ! { dg-error "var = var op expr not mathematically equivalent to var = var op \\(expr\\)" }
  !$omp atomic capture
    v = x; x = x * 7 + 6   ! { dg-error "assignment must be var = var op expr or var = expr op var" }
  !$omp atomic capture
    v = x; x = ieor(x * 7, 6)   ! { dg-error "intrinsic arguments except one must not reference 'x'" }
  !$omp atomic capture
    v = x; x = x - 8 + 6   ! { dg-error "var = var op expr not mathematically equivalent to var = var op \\(expr\\)" }
  !$omp atomic capture
    v = x; x = ior (ieor(x, 7), 2)   ! { dg-error "intrinsic arguments except one must not reference 'x'" }
  !$omp atomic capture
    v = x; x = x / 7 * 2   ! { dg-error "var = var op expr not mathematically equivalent to var = var op \\(expr\\)" }
  !$omp atomic capture
    v = x; x = x / 7 / 2   ! { dg-error "var = var op expr not mathematically equivalent to var = var op \\(expr\\)" }
  !$omp atomic capture
    x = x * 7 + 6; v = x   ! { dg-error "assignment must be var = var op expr or var = expr op var" }
  !$omp atomic capture
    x = ieor(x * 7, 6); v = x   ! { dg-error "intrinsic arguments except one must not reference 'x'" }
  !$omp atomic capture
    x = x - 8 + 6; v = x   ! { dg-error "var = var op expr not mathematically equivalent to var = var op \\(expr\\)" }
  !$omp atomic capture
    x = ior(ieor(x, 7), 2); v = x   ! { dg-error "intrinsic arguments except one must not reference 'x'" }
end
