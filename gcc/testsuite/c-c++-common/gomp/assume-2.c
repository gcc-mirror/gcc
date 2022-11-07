void
foo (int i, int *a)
{
  #pragma omp assume no_openmp no_openmp			/* { dg-error "too many 'no_openmp' clauses" } */
  ;
  #pragma omp assume no_openmp_routines, no_openmp_routines	/* { dg-error "too many 'no_openmp_routines' clauses" } */
  ;
  #pragma omp assume no_parallelism, no_parallelism		/* { dg-error "too many 'no_parallelism' clauses" } */
  ;
  #pragma omp assume absent (target, target)			/* { dg-error "'target' directive mentioned multiple times in 'absent' clauses" } */
  ;
  #pragma omp assume absent (target, teams) absent (teams, parallel)	/* { dg-error "'teams' directive mentioned multiple times in 'absent' clauses" } */
  ;
  #pragma omp assume contains (cancellation point, cancellation point)	/* { dg-error "'cancellation point' directive mentioned multiple times in 'contains' clauses" } */
  ;
  #pragma omp assume contains (target enter data, target exit data) contains (target exit data, parallel)	/* { dg-error "target exit data' directive mentioned multiple times in 'contains' clauses" } */
  ;
  #pragma omp assume absent (target enter data, target exit data) contains (target exit data, parallel)		/* { dg-error "'target exit data' directive mentioned in both 'absent' and 'contains' clauses" } */
  ;
  #pragma omp assume contains (target enter data, target exit data) absent (target enter data, parallel)	/* { dg-error "'target enter data' directive mentioned in both 'absent' and 'contains' clauses" } */
  ;
  #pragma omp assume contains (declare target)			/* { dg-error "unknown OpenMP directive name in 'contains' clause argument" } */
  ;
  #pragma omp assume absent (parallel for simd)			/* { dg-error "unknown OpenMP directive name in 'absent' clause argument" } */
  ;
  #pragma omp assume contains (target parallel)			/* { dg-error "unknown OpenMP directive name in 'contains' clause argument" } */
  ;
  #pragma omp assume absent (assume)				/* { dg-error "unknown OpenMP directive name in 'absent' clause argument" } */
  ;
  #pragma omp assume absent (assumes)				/* { dg-error "unknown OpenMP directive name in 'absent' clause argument" } */
  ;
  #pragma omp assume contains (begin assumes)			/* { dg-error "unknown OpenMP directive name in 'contains' clause argument" } */
  ;
  #pragma omp assume contains (end assumes)			/* { dg-error "unknown OpenMP directive name in 'contains' clause argument" } */
  ;
  #pragma omp assume contains (foo)				/* { dg-error "unknown OpenMP directive name in 'contains' clause argument" } */
  ;
  #pragma omp assume absent (target enter something)		/* { dg-error "unknown OpenMP directive name in 'absent' clause argument" } */
  ;
  #pragma omp assume foobar					/* { dg-error "expected assumption clause" } */
  ;
  #pragma omp assume ext_GCC_foobarbaz, ext_GCC_baz (1, 12, 1 < 17), no_parallelism	/* { dg-warning "unknown assumption clause 'ext_GCC_foobarbaz'" } */
  ;								/* { dg-warning "unknown assumption clause 'ext_GCC_baz'" "" { target *-*-* } .-1 } */
  #pragma omp assume						/* { dg-error "expected at least one assumption clause" } */
  ;
}
