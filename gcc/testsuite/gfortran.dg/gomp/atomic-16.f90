! { dg-do compile }
! { dg-options "-fopenmp" }

module m
  implicit none
  integer :: x = 6
contains

subroutine foo ()
  integer v
  !$omp atomic seq_cst read
  v = x
  !$omp atomic seq_cst, read
  v = x
  !$omp atomic seq_cst write
  x = v
  !$omp atomic seq_cst ,write
  x = v
  !$omp atomic seq_cst update
  x = x + v;
  !$omp atomic seq_cst , update
  x = v + x;
  !$omp atomic seq_cst capture
  v = x; x = x + 2;
  !$omp atomic seq_cst, capture
  v = x; x = 2 + x;
  !$omp atomic read , seq_cst
  v = x
  !$omp atomic write ,seq_cst
  x = v
  !$omp atomic update, seq_cst
  x = x + v
  !$omp atomic capture, seq_cst
  x = x + 2; v = x
end
end module m
