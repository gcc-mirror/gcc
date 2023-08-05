/* { dg-do run } */
/* { dg-options "-O2 -fno-tree-vectorize -fno-inline -fno-tree-loop-distribute-patterns -fpredictive-commoning -fdump-tree-pcom-details-blocks" } */

int arr1[105] = {2, 3, 5, 7, 11, 13, 0};
int arr2[105] = {2, 3, 5, 7, 11, 13, 0};
int arr3[105] = {2, 3, 5, 7, 11, 13, 0};
int arr4[105] = {2, 3, 5, 7, 11, 13, 0};
int result1[105] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -1, -2, -3, 0};
int result2[105] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -1, -1, -2, 0};
int result3[105] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -1, -2, -2, 0};
int result4[105] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -1, -1, -1, 0};

extern void abort (void);

void check (int *a, int *res, int len)
{
  int i;

  for (i = 0; i < len; i++)
    if (a[i] != res[i])
      abort ();
}

void __attribute__((noinline)) foo1 (int *a)
{
  int i;
  for (i = 0; i < 100; i++)
    {
      a[i] = 0;
      a[i + 1] = -1;
      a[i + 2] = -2;
      a[i + 3] = -3;
    }
}

void __attribute__((noinline)) foo2 (int *a)
{
  int i;
  for (i = 0; i < 100; i++)
    {
      a[i] = 0;
      a[i + 2] = -1;
      a[i + 3] = -2;
    }
}

void __attribute__((noinline)) foo3 (int *a)
{
  int i;
  for (i = 0; i < 100; i++)
    {
      a[i] = 0;
      a[i + 1] = -1;
      a[i + 3] = -2;
    }
}

void __attribute__((noinline)) foo4 (int *a)
{
  int i;
  for (i = 0; i < 100; i++)
    {
      a[i] = 0;
      a[i + 3] = -1;
    }
}

int main (void)
{
  foo1 (arr1);
  check (arr1, result1, 105);

  foo2 (arr2);
  check (arr2, result2, 105);

  foo3 (arr3);
  check (arr3, result3, 105);

  foo4 (arr4);
  check (arr4, result4, 105);

  return 0;
}
/* { dg-final { scan-tree-dump-times "Store-stores chain" 4 "pcom"} } */
/* { dg-final { scan-tree-dump-not "Invalid sum" "pcom" } } */
