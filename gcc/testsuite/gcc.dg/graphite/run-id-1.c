void abort (void);

void foo (int N)
{
  int i, j;
  int x[10000][10000];

  for (i = 0; i < N; i++)
    for (j = 0; j < N; j++)
      x[i][j] = i + j + 3;

  for (i = 0; i < N; i++)
    for (j = 0; j < N; j++)
      if (x[i][j] != i + j + 3)
	abort ();
}

int main(void)
{
  foo (10000);

  return 0;
}

