void
foo (int *a)
{
  int i, r = 0, s = 0;
  #pragma omp taskgroup task_reduction(+:r)
  #pragma omp parallel masked taskloop in_reduction(+:r)	/* { dg-error "'in_reduction' is not valid for '#pragma omp parallel masked taskloop'" } */
  for (i = 0; i < 64; i++)
    r += a[i];
  #pragma omp taskgroup task_reduction(+:s)
  #pragma omp parallel masked taskloop simd in_reduction(+:s)	/* { dg-error "'in_reduction' is not valid for '#pragma omp parallel masked taskloop simd'" } */
  for (i = 0; i < 64; i++)
    s += a[i];
}
