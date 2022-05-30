/* { dg-do compile } */
/* { dg-options "-O2 -funswitch-loops -fdump-tree-unswitch-optimized" } */

int
__attribute__((noipa))
foo(double *a, double *b, double *c, double *d, double *r, int size, int order)
{
  for (int i = 0; i < size; i++)
  {
    double tmp, tmp2;

    switch(order)
    {
      case 0:
        tmp = -8 * a[i];
        tmp2 = 2 * b[i];
        break;
      case 1: 
        tmp = 3 * a[i] -  2 * b[i];
        tmp2 = 5 * b[i] - 2 * c[i];
        break;
      case 2:
        tmp = 9 * a[i] +  2 * b[i] + c[i];
        tmp2 = 4 * b[i] + 2 * c[i] + 8 * d[i];
        break;
      case 3:
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

#define N 16 * 1024
double aa[N], bb[N], cc[N], dd[N], rr[N];

int main()
{
  for (int i = 0; i < 100 * 1000; i++)
    foo (aa, bb, cc, dd, rr, N, i % 4);
}


/* Test that we actually unswitched something.  */
/* { dg-final { scan-tree-dump-times "unswitching loop . on .switch. with condition: order.* == 0" 1 "unswitch" } } */
/* { dg-final { scan-tree-dump-times "unswitching loop . on .switch. with condition: order.* == 1" 1 "unswitch" } } */
/* { dg-final { scan-tree-dump-times "unswitching loop . on .switch. with condition: order.* == 2" 1 "unswitch" } } */
/* { dg-final { scan-tree-dump-times "unswitching loop . on .switch. with condition: order.* == 3" 1 "unswitch" } } */
