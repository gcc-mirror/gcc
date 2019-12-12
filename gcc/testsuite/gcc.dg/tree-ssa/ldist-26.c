/* { dg-do run } */
/* { dg-options "-O3 -ftree-loop-distribution -fdump-tree-ldist-details" } */

extern void abort (void);

int a[130], b[128], c[128];

int __attribute__((noinline,noclone))
foo (int len, int x)
{
  int i;
  for (i = 1; i <= len; ++i)
    {
      a[i] = a[i + 2] + 1;
      b[i] = 0;
      a[i + 1] = a[i] - 3;
      if (i < x)
	c[i] = a[i];
    }
  return i;
}

int main()
{
  int i;
  for (i = 0; i < 130; ++i)
    a[i] = i;
  foo (127, 67);
  if (a[0] != 0 || a[1] != 4 || a[127] != 130)
    abort ();
  return 0;
}

/* Loop splitting splits the iteration space so we end up with two
   loops entering loop distribution.  Both should have the b[i] = 0
   part split out as memset.  */
/* { dg-final { scan-tree-dump-times "distributed: split to 1 loops and 1 library calls" 2 "ldist" } } */
/* { dg-final { scan-tree-dump-times "generated memset zero" 2 "ldist" } } */
