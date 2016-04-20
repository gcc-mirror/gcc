/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-ivopts-details" } */

int a[128];
extern int b[];

int bar (int *);

int
foo (int n)
{
  int i;

  for (i = 0; i < n; i++)
    {
      unsigned char uc = (unsigned char)i;
      a[i] = i;
      b[uc] = 0;
    }

  bar (a);
  return 0;
}

/* Address of array reference to b is scev.  */
/* { dg-final { scan-tree-dump-times "use \[0-9\]\n  address" 2 "ivopts" } } */


