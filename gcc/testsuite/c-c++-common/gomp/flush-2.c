int a, b;

void
foo (void)
{
  #pragma omp flush
  #pragma omp flush (a, b)
  #pragma omp flush acquire
  #pragma omp flush release
  #pragma omp flush acq_rel
  #pragma omp flush relaxed		/* { dg-error "expected 'acq_rel', 'release' or 'acquire'" } */
  #pragma omp flush seq_cst		/* { dg-error "expected 'acq_rel', 'release' or 'acquire'" } */
  #pragma omp flush foobar		/* { dg-error "expected 'acq_rel', 'release' or 'acquire'" } */
  #pragma omp flush acquire (a, b)	/* { dg-error "'flush' list specified together with memory order clause" } */
  #pragma omp flush release (a, b)	/* { dg-error "'flush' list specified together with memory order clause" } */
  #pragma omp flush acq_rel (a, b)	/* { dg-error "'flush' list specified together with memory order clause" } */
}
