/* { dg-do compile } */
/* { dg-options "-O2 -funswitch-loops -fdump-tree-unswitch-optimized" } */

int
foo(double *a, double *b, double *c, double *d, double *r, int size, int order)
{
  for (int i = 0; i < size; i++)
  {
    double tmp;

    if (order == 1)
      tmp = -8 * a[i];
    else
      {
	if (order == 2)
	  tmp = -4 * b[i];
	else
	  tmp = a[i];
      }

    r[i] = 3.4f * tmp + d[i];
  }

  return 0;
}

/* { dg-final { scan-tree-dump-times "unswitching loop . on .if. with condition: order" 2 "unswitch" } } */
