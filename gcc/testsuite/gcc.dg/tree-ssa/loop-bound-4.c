/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-ivopts-details" } */

int *a;

int
foo (signed char s, signed char l)
{
  signed char i;
  int sum = 0;

  for (i = s; i > l; i--)
    {
      sum += a[i];
    }

  return sum;
}

/* Check loop niter bound information.  */
/* { dg-final { scan-tree-dump "bounded by 254" "ivopts" } } */
/* { dg-final { scan-tree-dump-not "bounded by 255" "ivopts" } } */
/* { dg-final { scan-tree-dump-not "zero if " "ivopts" } } */
