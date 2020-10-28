module m
  integer :: a, b
end module m

subroutine foo (void)
  use m
  !$omp flush
  !$omp flush (a, b)
  !$omp flush acquire
  !$omp flush release
  !$omp flush acq_rel
  !$omp flush relaxed		! { dg-error "Expected AQC_REL, RELEASE, or ACQUIRE" }
  !$omp flush seq_cst		! { dg-error "Expected AQC_REL, RELEASE, or ACQUIRE" }
  !$omp flush foobar		! { dg-error "Expected AQC_REL, RELEASE, or ACQUIRE" }
  !$omp flush acquire (a, b)	! { dg-error "List specified together with memory order clause in FLUSH directive" }
  !$omp flush release (a, b)	! { dg-error "List specified together with memory order clause in FLUSH directive" }
  !$omp flush acq_rel (a, b)	! { dg-error "List specified together with memory order clause in FLUSH directive" }
end
