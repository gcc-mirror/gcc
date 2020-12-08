/* PR c++/98187 */
/* { dg-do compile } */
/* { dg-options "-fopenmp-simd -O2 -fdump-tree-gimple" } */
/* { dg-final { scan-tree-dump-times "#pragma omp simd" 17 "gimple" } } */

void
foo (int *p)
{
  int i;
  #pragma omp distribute parallel for
  for (i = 0; i < 64; i++)
    p[i]++;
  #pragma omp distribute parallel for simd
  for (i = 0; i < 64; i++)
    p[i]++;
  #pragma omp distribute simd
  for (i = 0; i < 64; i++)
    p[i]++;
}

void
bar (int *p)
{
  int i;
  #pragma omp for simd
  for (i = 0; i < 64; i++)
    p[i]++;
  #pragma omp master taskloop
  for (i = 0; i < 64; i++)
    p[i]++;
  #pragma omp master taskloop simd
  for (i = 0; i < 64; i++)
    p[i]++;
  #pragma omp parallel for
  for (i = 0; i < 64; i++)
    p[i]++;
  #pragma omp parallel for simd
  for (i = 0; i < 64; i++)
    p[i]++;
  #pragma omp parallel loop
  for (i = 0; i < 64; i++)
    p[i]++;
  #pragma omp parallel master
  p[0]++;
  #pragma omp parallel master taskloop
  for (i = 0; i < 64; i++)
    p[i]++;
  #pragma omp parallel master taskloop simd
  for (i = 0; i < 64; i++)
    p[i]++;
  #pragma omp parallel sections
  {
    p[0]++;
    #pragma omp section
    p[1]++;
    #pragma omp section
    p[2]++;
  }
  #pragma omp target parallel
  #pragma omp master
  p[0]++;
  #pragma omp target parallel for
  for (i = 0; i < 64; i++)
    p[i]++;
  #pragma omp target parallel for simd
  for (i = 0; i < 64; i++)
    p[i]++;
  #pragma omp target parallel loop
  for (i = 0; i < 64; i++)
    p[i]++;
  #pragma omp target teams private (i)
  i = 0;
  #pragma omp target teams distribute
  for (i = 0; i < 64; i++)
    p[i]++;
  #pragma omp target teams distribute parallel for
  for (i = 0; i < 64; i++)
    p[i]++;
  #pragma omp target teams distribute parallel for simd
  for (i = 0; i < 64; i++)
    p[i]++;
  #pragma omp target teams distribute simd
  for (i = 0; i < 64; i++)
    p[i]++;
  #pragma omp target teams loop
  for (i = 0; i < 64; i++)
    p[i]++;
  #pragma omp target simd
  for (i = 0; i < 64; i++)
    p[i]++;
  #pragma omp taskloop simd
  for (i = 0; i < 64; i++)
    p[i]++;
  #pragma omp teams distribute
  for (i = 0; i < 64; i++)
    p[i]++;
  #pragma omp teams distribute parallel for
  for (i = 0; i < 64; i++)
    p[i]++;
  #pragma omp teams distribute parallel for simd
  for (i = 0; i < 64; i++)
    p[i]++;
  #pragma omp teams distribute simd
  for (i = 0; i < 64; i++)
    p[i]++;
  #pragma omp teams loop
  for (i = 0; i < 64; i++)
    p[i]++;
}
