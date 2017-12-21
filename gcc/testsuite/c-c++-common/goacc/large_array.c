/* Ensure that alloca'ed arrays can be transferred to the
   accelerator. */

/* { dg-require-effective-target alloca } */

int
main ()
{
  int n = 100, m = 10, i, j;
  float a[n][m];

  #pragma acc parallel loop
  for (i = 0; i < n; i++)
    for (j = 0; j < m; j++)
      a[i][j] = 0;

  return 0;
}
