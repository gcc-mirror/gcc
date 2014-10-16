void bar (int);

void
foo (int N, int k, int *fb)
{
  int i, j;
  for (i = 1; i <= N; i++)
    {
      for (j = 1; j < i; j++)
	k %= N;
      bar (k);
    }
}
