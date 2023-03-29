#pragma omp end assumes	/* { dg-error "'#pragma omp end assumes' without corresponding '#pragma omp begin assumes'" } */
void foo (void);
