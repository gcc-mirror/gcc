/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-ivopts-details" } */

int *a;

int
foo (signed char s)
{
  signed char i;
  int sum = 0;

  for (i = s; i > 0; i--)
    {
      sum += a[i];
    }

  return sum;
}

/* Check loop niter bound information.  */
/* { dg-final { scan-tree-dump "bounded by 126" "ivopts" } } */
/* { dg-final { scan-tree-dump-not "bounded by 127" "ivopts" } } */
/* { dg-final { scan-tree-dump-not "zero if " "ivopts" } } */
