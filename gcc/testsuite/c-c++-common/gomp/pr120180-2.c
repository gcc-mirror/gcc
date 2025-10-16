/* { dg-do compile } */

/* This test case checks that a non-executable OpenMP directive is accepted 
   as intervening code.  */

int
test1 ()
{
  int blksize = 15000;
  double *qq;
  int i, k, nq;
#pragma omp target parallel for collapse(2) map(qq[ : 0]) private(i)
  for (k = 0; k < blksize; k++)
    {
#pragma omp nothing
      for (i = 0; i < nq; i++)
	qq[k * nq + i] = 0.0;
    }
  return 0;
}

int
test2 ()
{
  int i, k, m, n;
  double *qq, x, z;
#pragma omp for collapse(2)
  for (i = 1; i < n; i++)
    {
#pragma omp assume holds(x > 1)
      z = __builtin_fabs (x - i);
      for (k = 0; k < m; k++)
	qq[k * m + i] = z;
    }
  return 0;
}

int
test3 ()
{
  int i, k, m, n;
  double *qq, z;
#pragma omp for collapse(2)
  for (i = 1; i < n; i++)
    {
#pragma omp error at(compilation) /* { dg-error "'pragma omp error' encountered" } */
      for (k = 0; k < m; k++)
	qq[k * m + i] = z;
    }
  return 0;
}

int
test4 ()
{
  int i, k, m, n;
  double *qq, z;
#pragma omp for collapse(2)
  for (i = 1; i < n; i++)
    {
#pragma omp error at(execution) /* { dg-error "pragma omp error' with 'at\\(execution\\)' clause may not be used in intervening code" } */
      for (k = 0; k < m; k++)
	qq[k * m + i] = z;
    }
  return 0;
}
