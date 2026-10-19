/* { dg-do compile } */
/* { dg-additional-options "-fdump-tree-original -w" } */

// PR middle-end/127723

void f(int n, int *a, double *d) {
  #pragma omp simd simdlen(scaled(int,1): 1) // { dg-message "sorry, unimplemented: 'simdlen' clause with 'scaled' modifier" }
  for (int i = 0; i < n; i++)
    a[i] += 5;
  #pragma omp simd simdlen(scaled(int): 2) // { dg-message "sorry, unimplemented: 'simdlen' clause with 'scaled' modifier" }
  for (int i = 0; i < n; i++)
    a[i] += 5;

  #pragma omp simd simdlen(scaled(double, 4): 8) // { dg-message "sorry, unimplemented: 'simdlen' clause with 'scaled' modifier" }
  for (int i = 0; i < n; i++)
    d[i] = 0.0;
}

/* { dg-final { scan-tree-dump "#pragma omp simd simdlen\\(scaled\\(int,1\\):1\\)" "original" } }  */
/* { dg-final { scan-tree-dump "#pragma omp simd simdlen\\(scaled\\(int\\):2\\)" "original" } }  */
/* { dg-final { scan-tree-dump "#pragma omp simd simdlen\\(scaled\\(double,4\\):8\\)" "original" } }  */
