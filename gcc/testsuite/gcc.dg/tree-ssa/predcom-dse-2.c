/* { dg-do run } */
/* { dg-options "-O2 -fno-inline -fno-tree-loop-distribute-patterns -fpredictive-commoning -fdump-tree-pcom-details" } */

int arr[105] = {2, 3, 5, 7, 11};
int result0[10] = {2, 3, 5, 7, 11};
int result1[10] = {0, 3, -1, -2, 11, 0};
int result2[10] = {0, 0, -1, -1, -2, 0};
int result3[10] = {0, 0, 0, -1, -1, -2, 0};
int result4[10] = {0, 0, 0, 0, -1, -1, -2, 0};
int result100[105] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -1, -1, -2, 0};

extern void abort (void);

void __attribute__((noinline)) foo (int *a, int len)
{
  int i;
  for (i = 0; i < len; i++)
    {
      a[i] = 0;
      a[i + 2] = -1;
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
  foo (arr, 0);
  check (arr, result0, 10);

  foo (arr, 1);
  check (arr, result1, 10);

  foo (arr, 2);
  check (arr, result2, 10);

  foo (arr, 3);
  check (arr, result3, 10);

  foo (arr, 4);
  check (arr, result4, 10);

  foo (arr, 100);
  check (arr, result100, 105);

  return 0;
}
/* { dg-final { scan-tree-dump "Store-stores chain" "pcom"} } */
