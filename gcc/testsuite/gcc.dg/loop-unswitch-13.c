/* { dg-do compile } */
/* { dg-options "-O2 -funswitch-loops -fdump-tree-unswitch-optimized" } */

int
foo(double *a, double *b, double *c, double *d, double *r, int size, unsigned order)
{
  for (int i = 0; i < size; i++)
  {
    double tmp;

    switch (order)
      {
      case 0 ... 4:
	tmp = -8 * a[i];
	break;
      default:
	tmp = -4 * b[i];
	break;
      }

    double x = 3 * tmp + d[i] + tmp;

    /* This should not be unswitched as it's mutually excluded with case 0 ... 4.  */
    if (order >= 5)
      x += 2;

    double y = 3.4f * tmp + d[i];
    r[i] = x + y;
  }

  return 0;
}

/* { dg-final { scan-tree-dump-times "Unswitching loop on condition: order.* <= 4" 1 "unswitch" } } */
