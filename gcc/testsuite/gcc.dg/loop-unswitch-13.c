/* { dg-do compile } */
/* { dg-options "-O2 -funswitch-loops -fno-thread-jumps -fdump-tree-unswitch-optimized" } */

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

    /* This and the case 0 ... 4 condition should only be unswitched once
       since they are mutually excluded.  */
    if (order >= 5)
      x += 2;

    double y = 3.4f * tmp + d[i];
    r[i] = x + y;
  }

  return 0;
}

/* { dg-final { scan-tree-dump-times "unswitching loop . on .\[^\n\r\]*. with condition" 1 "unswitch" } } */
