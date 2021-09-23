#pragma omp requires atomic_default_mem_order(acquire)	/* { dg-error "expected 'seq_cst', 'relaxed' or 'acq_rel'" } */
#pragma omp requires atomic_default_mem_order(release)	/* { dg-error "expected 'seq_cst', 'relaxed' or 'acq_rel'" } */
#pragma omp requires atomic_default_mem_order(foobar)	/* { dg-error "expected 'seq_cst', 'relaxed' or 'acq_rel'" } */
#pragma omp requires atomic_default_mem_order (	/* { dg-error "expected 'seq_cst', 'relaxed' or 'acq_rel'" } */
/* { dg-error "expected '\\\)' before end of line" "" { target *-*-* } .-1 } */
#pragma omp requires atomic_default_mem_order(seq_cst),	/* { dg-error "expected end of line before ',' token" } */
