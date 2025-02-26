/* { dg-do run } */
/* { dg-options "-O2 -fno-inline -fpredictive-commoning -fdump-tree-pcom-details-blocks" } */

int arr[105] = {2, 3, 5, 7, 11};
int x[105] = {2, 3, 5, 7, 11};
int result0[10] = {2, 3, 5, 7, 11};
int result1[10] = {0, 0, 0, 0, 0, 0, -1, -2, -2};

extern void abort (void);

int __attribute__((noinline)) foo (int * __restrict__ a, int * __restrict__ b, int len, int flag)
{
  int i, sum = 0;
  for (i = 0; i < len; i++)
    {
      a[i] = 0;
      b[i + 2] = i;
      a[i + 1] = -1;
      sum += b[i];
      a[i + 3] = -2;
    }
  return sum;
}

void check (int *a, int *res, int len, int sum, int val)
{
  int i;

  if (sum != val)
    abort ();

#pragma GCC novector
  for (i = 0; i < len; i++)
    if (a[i] != res[i])
      abort ();
}

int main (void)
{
  int i, sum;

  sum = foo (arr, x, 0, 0);
  check (arr, result0, 10, sum, 0);

  sum = foo (arr, x, 6, 0);
  check (arr, result1, 10, sum, 11);

  return 0;
}
/* { dg-final { scan-tree-dump "Store-stores chain" "pcom"} } */
/* { dg-final { scan-tree-dump "Store-loads chain" "pcom"} } */
/* { dg-final { scan-tree-dump-not "Invalid sum" "pcom" } } */

