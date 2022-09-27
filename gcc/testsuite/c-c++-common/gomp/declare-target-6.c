#pragma omp end declare target	/* { dg-error "'#pragma omp end declare target' without corresponding '#pragma omp declare target'" } */
void foo (void);
