/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-ivopts-details" } */

int *a;

int
foo (unsigned char s, unsigned char l)
{
  unsigned char i;
  int sum = 0;

  for (i = s; i < l; i += 1)
    {
      sum += a[i];
    }

  return sum;
}

/* Address of array reference is scev.  */
/* { dg-final { scan-tree-dump-times "use \[0-9\]\n  address" 1 "ivopts" } } */

