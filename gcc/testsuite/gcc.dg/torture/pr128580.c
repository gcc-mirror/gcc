/* { dg-do run } */
/* { dg-options "-O3 -fno-tree-loop-im -fno-tree-pre" } */

#define N 4

int a[N * N];

__attribute__((noipa)) void
f (int n)
{
  for (int i = 0; i < n; i++)
    for (int k = 0; k < n; k++)
      for (int j = 0; j < n; j++)
	a[i * k + j] = i;
}

int
main (void)
{
  f (N);
  /* For i == N - 1, i * k + j covers 0 ... (N - 1) * N.  */
  for (int x = 0; x < N * N; x++)
    if (a[x] != (x <= (N - 1) * N ? N - 1 : 0))
      __builtin_abort ();
  return 0;
}
