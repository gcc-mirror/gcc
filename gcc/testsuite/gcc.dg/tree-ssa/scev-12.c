/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-ivopts-details" } */

int a[128];
extern int b[];

int bar (int *);

int
foo (int x, int n)
{
  int i;

  for (i = 0; i < n; i++)
    {
      unsigned char uc = (unsigned char)i;
      if (x)
	a[i] = i;
      b[uc] = 0;
    }

  bar (a);
  return 0;
}

/* Address of array reference to b is not scev.  */
/* { dg-final { scan-tree-dump-times "  Type:\\tREFERENCE ADDRESS\n" 1 "ivopts" } } */
