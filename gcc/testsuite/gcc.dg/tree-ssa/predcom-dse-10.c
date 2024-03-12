/* { dg-do run } */
/* { dg-options "-O2 -fno-inline -fpredictive-commoning -fdump-tree-pcom-details-blocks" } */

int arr[105] = {2, 3, 5, 7, 11};
int result0[10] = {2, 3, 5, 7, 11};
int result1[10] = {0, 3, 5, -2, 11, 0};

extern void abort (void);

void __attribute__((noinline)) foo (int *a, int len, int flag)
{
  int i;
  for (i = 0; i < len; i++)
    {
      a[i] = 0;
      if (flag)
        a[i + 1] = -1;
      a[i + 3] = -2;
    }
}

void check (int *a, int *res, int len)
{
  int i;

  for (i = 0; i < len; i++)
    if (a[i] != res[i])
      abort ();
}

int main (void)
{
  int i;

  foo (arr, 0, 0);
  check (arr, result0, 10);

  foo (arr, 1, 0);
  check (arr, result1, 10);

  return 0;
}
/* { dg-final { scan-tree-dump-not "Store-stores chain" "pcom"} } */
/* { dg-final { scan-tree-dump-not "Invalid sum" "pcom" } } */
