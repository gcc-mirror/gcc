#pragma omp requires atomic_default_mem_order(acquire)	/* { dg-error "expected 'seq_cst', 'relaxed' or 'acq_rel'" } */
#pragma omp requires atomic_default_mem_order(release)	/* { dg-error "expected 'seq_cst', 'relaxed' or 'acq_rel'" } */
#pragma omp requires atomic_default_mem_order(foobar)	/* { dg-error "expected 'seq_cst', 'relaxed' or 'acq_rel'" } */
