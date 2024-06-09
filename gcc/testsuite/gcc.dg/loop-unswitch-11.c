/* { dg-do compile } */
/* { dg-options "-O2 -funswitch-loops -fdump-tree-unswitch-optimized" } */
/* { dg-require-effective-target int32 } */

int
foo(double *a, double *b, double *c, double *d, double *r, int size, int order)
{
  for (int i = 0; i < size; i++)
  {
    double tmp, tmp2;

    switch(order)
    {
      case 5 ... 6:
      case 9:
        tmp = -8 * a[i];
        tmp2 = 2 * b[i];
        break;
      case 11: 
        tmp = 3 * a[i] -  2 * b[i];
        tmp2 = 5 * b[i] - 2 * c[i];
        break;
      case 22:
        tmp = 9 * a[i] +  2 * b[i] + c[i];
        tmp2 = 4 * b[i] + 2 * c[i] + 8 * d[i];
        break;
      case 33:
        tmp = 3 * a[i] +  2 * b[i] - c[i];
        tmp2 = b[i] - 2 * c[i] + 8 * d[i];
        break;
      default:
        __builtin_unreachable ();
    }

    double x = 3 * tmp + d[i] + tmp;
    double y = 3.4f * tmp + d[i] + tmp2;
    r[i] = x + y;
  }

  return 0;
}

/* { dg-final { scan-tree-dump-times "unswitching loop . on .switch. with condition: order.* \\+ 4294967291.*order.* == 9" 1 "unswitch" } } */
/* { dg-final { scan-tree-dump-times "unswitching loop . on .switch. with condition: order.* == 1" 1 "unswitch" } } */
/* { dg-final { scan-tree-dump-times "unswitching loop . on .switch. with condition: order.* == 2" 1 "unswitch" } } */
/* { dg-final { scan-tree-dump-times "unswitching loop . on .switch. with condition: order.* == 3" 1 "unswitch" } } */
