/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-ivopts-details" } */

int *a;

int
foo (signed char s, signed char l)
{
  signed char i;
  int sum = 0;

  for (i = s; i < l; i++)
    {
      sum += a[i];
    }

  return sum;
}

/* Address of array reference is scev.  */
/* { dg-final { scan-tree-dump-times "  Type:\\tADDRESS\n  Use \[0-9\].\[0-9\]:" 1 "ivopts" } } */

