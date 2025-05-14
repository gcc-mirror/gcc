/* { dg-do run } */
/* { dg-options "-O2 -fno-inline -fpredictive-commoning -fdump-tree-pcom-details-blocks" } */

int arr[105] = {2, 3, 5, 7, 11};
int result0[10] = {2, 3, 5, 7, 11};
int result1[10] = {0, -1, 5, -2, 11, 0};
int result2[10] = {0, 0, -1, -2, -2, 0};
int result3[10] = {0, 0, 0, -1, -2, -2, 0};
int result4[10] = {0, 0, 0, 0, -1, -2, -2, 0};
int result100[105] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -1, -2, -2, 0};

extern void abort (void);
int sum;

void __attribute__((noinline)) foo (int * restrict a, int len, int t1, int t2)
{
  int i;
  for (i = 0; i < len; i++)
    {
      a[i] = t1;
      a[i + 3] = t2;
      a[i + 1] = -1;
      sum = sum + a[i] + a[i + 3];
    }
}

void check (int *a, int *res, int len, int sval)
{
  int i;

  if (sum != sval)
    abort ();

#pragma GCC novector
  for (i = 0; i < len; i++)
    if (a[i] != res[i])
      abort ();
}

int main (void)
{
  foo (arr, 0, 0, -2);
  check (arr, result0, 10, 0);

  foo (arr, 1, 0, -2);
  check (arr, result1, 10, -2);

  foo (arr, 2, 0, -2);
  check (arr, result2, 10, -6);

  foo (arr, 3, 0, -2);
  check (arr, result3, 10, -12);

  foo (arr, 4, 0, -2);
  check (arr, result4, 10, -20);

  foo (arr, 100, 0, -2);
  check (arr, result100, 105, -220);

  return 0;
}
/* { dg-final { scan-tree-dump "Store-stores chain" "pcom"} } */
/* { dg-final { scan-tree-dump-not "Invalid sum" "pcom" } } */
