#pragma omp declare target
void foo (void);	/* { dg-error "'#pragma omp declare target' without corresponding '#pragma omp end declare target'" } */
