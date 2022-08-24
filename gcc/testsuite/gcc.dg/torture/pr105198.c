/* { dg-do run } */
/* { dg-additional-options "-fno-tree-pre -fpredictive-commoning" } */

static  __attribute__ ((noipa)) void
next_set(int *x, int n, int k)
{
  int j = k - 1;
  int tmp = x[j]++;
  while (j > 0)
    {
      if (x[j] < n - (k - 1 -j))
        break;
      j--;
      x[j]++;
      tmp = x[j];
    }
  if (tmp != 2 || j != 1 || x[0] != 0 || x[1] != 2 || x[2] != 5)
    __builtin_abort ();
}

int main()
{
  int x[3] = {0, 1, 4};
  next_set(x, 5, 3);
  return 0;
}
