#pragma omp assumes no_openmp no_openmp				/* { dg-error "too many 'no_openmp' clauses" } */
#pragma omp assumes no_openmp_routines, no_openmp_routines	/* { dg-error "too many 'no_openmp_routines' clauses" } */
#pragma omp assumes no_parallelism, no_parallelism		/* { dg-error "too many 'no_parallelism' clauses" } */
#pragma omp assumes absent (target, target)			/* { dg-error "'target' directive mentioned multiple times in 'absent' clauses" } */
#pragma omp assumes absent (target, teams) absent (teams, parallel)	/* { dg-error "'teams' directive mentioned multiple times in 'absent' clauses" } */
#pragma omp assumes contains (cancellation point, cancellation point)	/* { dg-error "'cancellation point' directive mentioned multiple times in 'contains' clauses" } */
#pragma omp assumes contains (target enter data, target exit data) contains (target exit data, parallel)	/* { dg-error "target exit data' directive mentioned multiple times in 'contains' clauses" } */
#pragma omp assumes absent (target enter data, target exit data) contains (target exit data, parallel)		/* { dg-error "'target exit data' directive mentioned in both 'absent' and 'contains' clauses" } */
#pragma omp assumes contains (target enter data, target exit data) absent (target enter data, parallel)	/* { dg-error "'target enter data' directive mentioned in both 'absent' and 'contains' clauses" } */
#pragma omp assumes contains (declare target)			/* { dg-error "invalid OpenMP directive name in 'contains' clause argument" } */
#pragma omp assumes absent (parallel for simd)			/* { dg-error "unknown OpenMP directive name in 'absent' clause argument" } */
#pragma omp assumes contains (target parallel)			/* { dg-error "unknown OpenMP directive name in 'contains' clause argument" } */
#pragma omp assumes absent (assume)				/* { dg-error "invalid OpenMP directive name in 'absent' clause argument" } */
#pragma omp assumes absent (assumes)				/* { dg-error "invalid OpenMP directive name in 'absent' clause argument" } */
#pragma omp assumes contains (begin assumes)			/* { dg-error "invalid OpenMP directive name in 'contains' clause argument" } */
#pragma omp assumes contains (end assumes)			/* { dg-error "invalid OpenMP directive name in 'contains' clause argument" } */
#pragma omp assumes contains (foo)				/* { dg-error "unknown OpenMP directive name in 'contains' clause argument" } */
#pragma omp assumes absent (target enter something)		/* { dg-error "unknown OpenMP directive name in 'absent' clause argument" } */
#pragma omp assumes foobar					/* { dg-error "expected assumption clause" } */
#pragma omp assumes ext_GCC_foobarbaz, ext_GCC_baz (1, 12, 1 < 17), no_parallelism	/* { dg-warning "unknown assumption clause 'ext_GCC_foobarbaz'" } */
								/* { dg-warning "unknown assumption clause 'ext_GCC_baz'" "" { target *-*-* } .-1 } */
#pragma omp assumes						/* { dg-error "expected at least one assumption clause" } */
int i;
