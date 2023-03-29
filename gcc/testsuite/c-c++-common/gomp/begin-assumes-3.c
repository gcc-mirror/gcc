#pragma omp begin assumes no_openmp_routines
void foo (void);	/* { dg-error "'#pragma omp begin assumes' without corresponding '#pragma omp end assumes'" } */
