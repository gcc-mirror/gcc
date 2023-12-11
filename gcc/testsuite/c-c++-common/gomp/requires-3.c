#pragma omp requires atomic_default_mem_order(foobar)	/* { dg-error "expected 'acq_rel', 'acquire', 'relaxed', 'release' or 'seq_cst'" } */
#pragma omp requires atomic_default_mem_order (	/* { dg-error "expected 'acq_rel', 'acquire', 'relaxed', 'release' or 'seq_cst'" } */
/* { dg-error "expected '\\\)' before end of line" "" { target *-*-* } .-1 } */
#pragma omp requires atomic_default_mem_order(seq_cst),	/* { dg-error "expected end of line before ',' token" } */
/* Valid since since 5.2, but ... */
#pragma omp requires atomic_default_mem_order(acquire)	/* { dg-error "more than one 'atomic_default_mem_order' clause in a single compilation unit" } */
