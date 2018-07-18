void foo (int n, double a, double *b, double *x)
{
  int i, j;

  if(n <= 0) return;
  if (a == 0.0e0) return;

  if (a > 5.0)
    {
      i = 0;
      goto sec;
    }
  for (i = 0; i < 1024; i++)
    {
      double y = b[i];
sec:
      b[i+1] = y + 5.0;
      for (j = 0; j < n; j++)
	x[j] = x[j] + a;
    }
}
