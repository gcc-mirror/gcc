// PR tree-optimization/87360
// { dg-do compile { target size32plus } }
// { dg-options "-O3 -fno-tree-dce --param unroll-jam-min-percent=0" }

void abort (void);

void foo (int N)
{
  int i, j;
  int x[1000][1000];

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
  foo (1000);

  return 0;
}
