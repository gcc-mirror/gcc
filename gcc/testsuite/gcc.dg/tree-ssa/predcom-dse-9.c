/* { dg-do run } */
/* { dg-options "-O2 -fno-inline -fpredictive-commoning -fdump-tree-pcom-details-blocks" } */

int arr1[105] = {2, 3, 5, 7, 11, 13, 17, 19};
int arr2[105] = {2, 3, 5, 7, 11, 13, 17, 19};
int arr3[105] = {2, 3, 5, 7, 11, 13, 17, 19};

int result1[105] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -1, -2, -3, -4, -5};
int result2[105] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -1, -3, -3, -5, -5};
int result3[105] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -5, -5, -5, -5, -5};

extern void abort (void);

void __attribute__((noinline)) foo1 (int *a)
{
  int i;
  for (i = 0; i < 100; i++)
    {
      a[i] = 0;
      a[i + 1] = -1;
      a[i + 2] = -2;
      a[i + 3] = -3;
      a[i + 4] = -4;
      a[i + 5] = -5;
    }
}

void __attribute__((noinline)) foo2 (int *a)
{
  int i;
  for (i = 0; i < 100; i++)
    {
      a[i] = 0;
      a[i + 1] = -1;
      a[i + 3] = -3;
      a[i + 5] = -5;
    }
}

void __attribute__((noinline)) foo3 (int *a)
{
  int i;
  for (i = 0; i < 100; i++)
    {
      a[i] = 0;
      a[i + 5] = -5;
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
  foo1 (arr1);
  check (arr1, result1, 10);

  foo2 (arr2);
  check (arr2, result2, 10);

  foo3 (arr3);
  check (arr3, result3, 10);

  return 0;
}
/* { dg-final { scan-tree-dump "Store-stores chain" "pcom"} } */
/* { dg-final { scan-tree-dump-not "Invalid sum" "pcom" } } */
