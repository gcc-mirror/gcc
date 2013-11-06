/* { dg-do compile } */
/* { dg-options "-fopenmp-simd -fdump-tree-original" } */

#pragma omp declare simd
float bar(float b) {
  return b*b;
}

void foo(int n, float *a, float *b)
{
  int i; 
#pragma omp simd
  for (i = 0; i < n ; i++)
    a[i] = b[i];
#pragma omp for simd
  for (i = 0; i < n ; i++)
    a[i] = b[i];
#pragma omp distribute simd
  for (i = 0; i < n ; i++)
    a[i] = b[i];
#pragma omp distribute parallel for simd
  for (i = 0; i < n ; i++)
    a[i] = b[i];
#pragma omp parallel for simd
  for (i = 0; i < n ; i++)
    a[i] = b[i];
#pragma omp teams distribute simd
  for (i = 0; i < n ; i++)
    a[i] = b[i];
#pragma omp target teams distribute simd
  for (i = 0; i < n ; i++)
    a[i] = b[i];
#pragma omp teams distribute parallel for simd
  for (i = 0; i < n ; i++)
    a[i] = b[i];
#pragma omp target teams distribute parallel for simd
  for (i = 0; i < n ; i++)
    a[i] = b[i];
}

/* { dg-final { scan-tree-dump-times "pragma omp simd" 9 "original" } } */
/* { dg-final { scan-tree-dump-not "omp for" "original" } } */
/* { dg-final { scan-tree-dump-not "omp distribute" "original" } } */
/* { dg-final { scan-tree-dump-not "omp teams" "original" } } */
/* { dg-final { scan-tree-dump-not "omp target" "original" } } */
/* { dg-final { scan-tree-dump-not "omp parallel" "original" } } */
