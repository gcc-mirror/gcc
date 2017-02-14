/* { dg-do compile } */
/* { dg-options "-O2 -ftree-loop-distribution -fdump-tree-ldist-details" } */

int foo (int flag, char *a)
{
  short i, j;
  short l = 0;
  if (flag == 1)
    l = 3;

  for (i = 0; i < 4; i++)
    {
      for (j = l - 1; j > 0; j--)
        a[j] = a[j - 1];
      a[0] = i;
    }
}

/* Addresses of array reference a[j] and a[j - 1] are SCEVs.  */
/* { dg-final { scan-tree-dump-not "failed: evolution of base is not affine." "ldist" } } */

