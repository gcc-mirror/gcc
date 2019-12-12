/* { dg-require-effective-target vect_double } */

#include "tree-vect.h"

void __attribute__((noinline,noclone))
foo (double *a4, int n)
{
  for (int i = 0; i < n; ++i)
    {
      /* We may not apply interleaving to the group (a), (b) because of (c).
         Instead group (d) and (b).  */
      double tem1 = a4[i*4] + a4[i*4+n*4] /* (a) */;
      double tem2 = a4[i*4+2*n*4+1];
      a4[i*4+n*4+1] = tem1; /* (c) */
      a4[i*4+1] = tem2;
      double tem3 = a4[i*4] - tem2;
      double tem4 = tem3 + a4[i*4+n*4] /* (d) */;
      a4[i*4+n*4+1] = tem4 + a4[i*4+n*4+1] /* (b) */;
    }
}
int main(int argc, char **argv)
{
  int n = 11;
  double a4[4 * n * 3];
  double a42[4 * n * 3];
  check_vect ();
  for (int i = 0; i < 4 * n * 3; ++i)
    {
      a4[i] = a42[i] = i;
      __asm__ volatile ("": : : "memory");
    }
  foo (a4, n);
  for (int i = 0; i < n; ++i)
    {
      double tem1 = a42[i*4] + a42[i*4+n*4];
      double tem2 = a42[i*4+2*n*4+1];
      a42[i*4+n*4+1] = tem1;
      a42[i*4+1] = tem2;
      double tem3 = a42[i*4] - tem2;
      double tem4 = tem3 + a42[i*4+n*4];
      a42[i*4+n*4+1] = tem4 + a42[i*4+n*4+1];
      __asm__ volatile ("": : : "memory");
    }
  for (int i = 0; i < 4 * n * 3; ++i)
    if (a4[i] != a42[i])
      __builtin_abort ();
  return 0;
}

/* For v2df we try to use SLP and fail miserably.  */
/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { target vect_sizes_32B_16B } } } */
