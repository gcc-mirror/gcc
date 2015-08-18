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

/* Check loop niter bound information.  */
/* { dg-final { scan-tree-dump "bounded by 254" "ivopts" } } */
/* { dg-final { scan-tree-dump-not "bounded by 255" "ivopts" } } */
