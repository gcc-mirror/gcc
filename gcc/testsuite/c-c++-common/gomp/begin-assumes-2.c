#pragma omp begin assumes no_openmp no_openmp			/* { dg-error "too many 'no_openmp' clauses" } */
void f1 (void) {}
#pragma omp end assumes
#pragma omp begin assumes no_openmp_routines, no_openmp_routines	/* { dg-error "too many 'no_openmp_routines' clauses" } */
void f2 (void) {}
#pragma omp end assumes
#pragma omp begin assumes no_parallelism, no_parallelism		/* { dg-error "too many 'no_parallelism' clauses" } */
void f3 (void) {}
#pragma omp end assumes
#pragma omp begin assumes absent (target, target)			/* { dg-error "'target' directive mentioned multiple times in 'absent' clauses" } */
void f4 (void) {}
#pragma omp end assumes
#pragma omp begin assumes absent (target, teams) absent (teams, parallel)	/* { dg-error "'teams' directive mentioned multiple times in 'absent' clauses" } */
void f5 (void) {}
#pragma omp end assumes
#pragma omp begin assumes contains (cancellation point, cancellation point)	/* { dg-error "'cancellation point' directive mentioned multiple times in 'contains' clauses" } */
void f6 (void) {}
#pragma omp end assumes
#pragma omp begin assumes contains (target enter data, target exit data) contains (target exit data, parallel)	/* { dg-error "target exit data' directive mentioned multiple times in 'contains' clauses" } */
void f7 (void) {}
#pragma omp end assumes
#pragma omp begin assumes absent (target enter data, target exit data) contains (target exit data, parallel)		/* { dg-error "'target exit data' directive mentioned in both 'absent' and 'contains' clauses" } */
void f8 (void) {}
#pragma omp end assumes
#pragma omp begin assumes contains (target enter data, target exit data) absent (target enter data, parallel)	/* { dg-error "'target enter data' directive mentioned in both 'absent' and 'contains' clauses" } */
void f9 (void) {}
#pragma omp end assumes
#pragma omp begin assumes contains (declare target)			/* { dg-error "unknown OpenMP directive name in 'contains' clause argument" } */
void f10 (void) {}
#pragma omp end assumes
#pragma omp begin assumes absent (parallel for simd)			/* { dg-error "unknown OpenMP directive name in 'absent' clause argument" } */
void f11 (void) {}
#pragma omp end assumes
#pragma omp begin assumes contains (target parallel)			/* { dg-error "unknown OpenMP directive name in 'contains' clause argument" } */
void f12 (void) {}
#pragma omp end assumes
#pragma omp begin assumes absent (assume)				/* { dg-error "unknown OpenMP directive name in 'absent' clause argument" } */
void f13 (void) {}
#pragma omp end assumes
#pragma omp begin assumes absent (assumes)				/* { dg-error "unknown OpenMP directive name in 'absent' clause argument" } */
void f14 (void) {}
#pragma omp end assumes
#pragma omp begin assumes contains (begin assumes)			/* { dg-error "unknown OpenMP directive name in 'contains' clause argument" } */
void f15 (void) {}
#pragma omp end assumes
#pragma omp begin assumes contains (end assumes)			/* { dg-error "unknown OpenMP directive name in 'contains' clause argument" } */
void f16 (void) {}
#pragma omp end assumes
#pragma omp begin assumes contains (foo)				/* { dg-error "unknown OpenMP directive name in 'contains' clause argument" } */
void f17 (void) {}
#pragma omp end assumes
#pragma omp begin assumes absent (target enter something)		/* { dg-error "unknown OpenMP directive name in 'absent' clause argument" } */
void f18 (void) {}
#pragma omp end assumes
#pragma omp begin assumes foobar					/* { dg-error "expected assumption clause" } */
void f19 (void) {}
#pragma omp end assumes
#pragma omp begin assumes ext_GCC_foobarbaz, ext_GCC_baz (1, 12, 1 < 17), no_parallelism	/* { dg-warning "unknown assumption clause 'ext_GCC_foobarbaz'" } */
void f20 (void) {}								/* { dg-warning "unknown assumption clause 'ext_GCC_baz'" "" { target *-*-* } .-1 } */
#pragma omp end assumes
#pragma omp begin assumes						/* { dg-error "expected at least one assumption clause" } */
void f21 (void) {}
#pragma omp end assumes
