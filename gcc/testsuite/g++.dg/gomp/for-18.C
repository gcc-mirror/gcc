// { dg-do compile }
extern int bar (int);

void
foo (void)
{
  int j, k = 1, l = 30, m = 4;
  long int o = 4;
  long long int p = 0;
#pragma omp for
  for (j = k; j <= l; j += m - 1)
    ;
#pragma omp for
  for (j = k; j <= l; j += (m - 1))
    ;
#pragma omp for
  for (j = k; j <= l; j += bar (m - 1))
    ;
#pragma omp for
  for (j = k; j <= l; j = j + m - 1)
    ;
#pragma omp for
  for (j = k; j <= l; j = j + (m - 1))
    ;
#pragma omp for
  for (j = k; j <= l; j = j + bar (m - 1))
    ;
#pragma omp for
  for (j = ({ int n; n = k; n; }); j <= l; j++)
    ;
#pragma omp for
  for (j = k; j <= ({ int n; n = l; n; }); j++)
    ;
#pragma omp for
  for (j = k; j <= l; j += ({ int n; n = 1; n; }))
    ;
#pragma omp for
  for (j = k; j <= l; j += m + 1)
    ;
#pragma omp for
  for (j = k; j <= l; j += o)
    ;
#pragma omp for
  for (j = k; j <= l; j = j + o)
    ;
#pragma omp for
  for (j = k; j <= l; j = o + 1 + j)
    ;
#pragma omp for
  for (j = k; j <= l; j = o + m + j)
    ;
#pragma omp for
  for (j = k; j <= l; j += o + p)
    ;
#pragma omp for
  for (j = k; j <= l; j = j + o + p)
    ;
#pragma omp for
  for (j = l; j >= k; j -= o)
    ;
#pragma omp for
  for (j = l; j >= k; j -= p)
    ;
#pragma omp for
  for (j = l; j >= k; j -= o + p)
    ;
}
