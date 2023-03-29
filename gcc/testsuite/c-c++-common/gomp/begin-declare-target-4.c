#pragma omp begin declare target
void foo (void);	/* { dg-error "'#pragma omp begin declare target' without corresponding '#pragma omp end declare target'" } */
