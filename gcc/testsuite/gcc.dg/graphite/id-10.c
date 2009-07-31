int bar[100][100];

int
foo (int N, unsigned int J)
{
  int i, k;

  for (k = 0; k < N; k++)
    if (k != J)
      for (i = 0; i < N; i++)
	if (i != J)
	  bar[k][i] = 20;

  return bar[N][J];
}
