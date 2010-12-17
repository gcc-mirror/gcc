/* { dg-do run } */
/* { dg-options "-O -fgraphite-identity" } */

#define N 128

int
main ()
{
  int arr[N], i, s = 0;
  for (i = 0; i < N; i++)
    arr[i] = i;

  for (i = 0; i < N; i++)
    if (arr[i] != i)
      __builtin_abort ();

  for (i = 0; i < N; i++)
    s += arr[i];
  if (s != (N * (N - 1)) / 2)
    __builtin_abort ();
  return 0;
}
