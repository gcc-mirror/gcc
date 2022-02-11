! { dg-do compile }

module m
implicit none
integer x
real d

contains

real function foo (y, e, f)
  integer :: y
  real v, e
  real(8) :: f
  !$omp atomic compare compare	! { dg-error "Duplicated 'compare' clause" }
  if (x == y) x = d
  !$omp atomic compare fail(seq_cst) fail(seq_cst)	! { dg-error "Duplicated 'fail' clause" }
  if (x == y) x = d
  !$omp atomic compare,fail(seq_cst),fail(relaxed)	! { dg-error "Duplicated 'fail' clause" }
  if (x == y) x = d
  !$omp atomic compare weak weak	! { dg-error "Duplicated 'weak' clause" }
  if (x == y) x = d
  !$omp atomic read capture	! { dg-error "CAPTURE clause is incompatible with READ or WRITE" }
  v = d
  !$omp atomic capture, write	! { dg-error "CAPTURE clause is incompatible with READ or WRITE" }
  d = v; v = v + 1              ! { dg-error "Unexpected ..OMP ATOMIC statement" "" { target *-*-* } .-1 }
  foo = v
end

real function bar (y, e, f)
  integer :: y
  real v, e
  real(8) :: f
  !$omp atomic read compare	! { dg-error "COMPARE clause is incompatible with READ or WRITE" }
  if (x == y) x = d
  !$omp atomic compare, write	! { dg-error "COMPARE clause is incompatible with READ or WRITE" }
  if (x == y) x = d
  !$omp atomic read fail(seq_cst)	! { dg-error "FAIL clause is incompatible with READ or WRITE" }
  v = d
  !$omp atomic fail(relaxed), write	! { dg-error "FAIL clause is incompatible with READ or WRITE" }
  d = v
  !$omp atomic fail(relaxed) update	! { dg-error "FAIL clause requiries either the COMPARE clause or using the intrinsic MIN/MAX procedure" }
  d = d + 3.0
  !$omp atomic fail(relaxed)	! { dg-error "FAIL clause requiries either the COMPARE clause or using the intrinsic MIN/MAX procedure" }
  d = d + 3.0
  !$omp atomic capture fail(relaxed)	! { dg-error "FAIL clause requiries either the COMPARE clause or using the intrinsic MIN/MAX procedure" }
  v = d; d = d + 3.0
  !$omp atomic read weak		! { dg-error "WEAK clause requires COMPARE clause" }
  v = d
  !$omp atomic weak, write	! { dg-error "WEAK clause requires COMPARE clause" }
  d = v
  !$omp atomic weak update	! { dg-error "WEAK clause requires COMPARE clause" }
  d = d + 3.0
  !$omp atomic weak		! { dg-error "WEAK clause requires COMPARE clause" }
  d = d + 3.0
  !$omp atomic capture weak	! { dg-error "WEAK clause requires COMPARE clause" }
  d = d + 3.0; v = d
  !$omp atomic capture
  d = d + 3.0; v = x            ! { dg-error "capture statement reads from different variable than update statement writes" }
  !$omp atomic compare fail	! { dg-error "Expected '\\\(' after 'fail'" }
  if (x == y) x = d
  !$omp atomic compare fail(	! { dg-error "Expected SEQ_CST, ACQUIRE or RELAXED" }
  if (x == y) x = d
  !$omp atomic compare fail()	! { dg-error "Expected SEQ_CST, ACQUIRE or RELAXED" }
  if (x == y) x = d
  !$omp atomic compare fail(foobar)	! { dg-error "Expected SEQ_CST, ACQUIRE or RELAXED" }
  if (x == y) x = d
  !$omp atomic compare fail(acq_rel)	! { dg-error "Expected SEQ_CST, ACQUIRE or RELAXED" }
  if (x == y) x = d
  !$omp atomic compare fail(release)	! { dg-error "Expected SEQ_CST, ACQUIRE or RELAXED" }
  if (x == y) x = d
  !$omp atomic compare fail(seq_cst	! { dg-error "Failed to match clause" }
  if (x == y) x = d
  bar = v
end

subroutine foobar
  implicit none
  integer :: i, j, k

  !$omp atomic compare write  ! { dg-error "COMPARE clause is incompatible with READ or WRITE" }
    if (i == 1) i = 5

  !$omp atomic compare
    if (k == 5) i = 7 ! { dg-error "For !.OMP ATOMIC COMPARE, the first operand in comparison at .1. must be the variable 'i' that the update statement writes into at .2." }

  !$omp atomic compare
    if (j == i) i = 8 ! { dg-error "For !.OMP ATOMIC COMPARE, the first operand in comparison at .1. must be the variable 'i' that the update statement writes into at .2." }

  !$omp atomic compare
    if (i == 5) i = 8

  !$omp atomic compare
    if (5 == i) i = 8 ! { dg-error "Expected scalar intrinsic variable at .1. in atomic comparison" }

  !$omp atomic compare
    if (i == 5) i = i + 8 ! { dg-error "20: expr in !.OMP ATOMIC COMPARE assignment var = expr must be scalar and cannot reference var" }

end subroutine
end module
