/* { dg-additional-options "-O3 -fno-fast-math" } */

#include "tree-vect.h"

double __attribute__((noinline,noclone))
compute_integral (double w_1[18])
{
  double A = 0;
  double t33[2][6] = {{0.0, 0.0, 0.0, 0.0, 0.0, 0.0},
	{0.0, 0.0, 0.0, 0.0, 0.0, 0.0}};
  double t43[2] = {0.0, 0.0};
  double t31[2][2] = {{1.0, 1.0}, {1.0, 1.0}};
  double t32[2][3] = {{0.0, 0.0, 1.0}, {0.0, 0.0, 1.0}};

  for (int ip_1 = 0; ip_1 < 2; ++ip_1)
    {
#pragma GCC unroll 0
      for (int i_0 = 0; i_0 < 6; ++i_0)
	t33[ip_1][i_0] = ((w_1[i_0*3] * t32[ip_1][0])
			  + (w_1[i_0*3+2] * t32[ip_1][2]));
      t43[ip_1] = 2.0;
    }
  for (int i_0 = 0; i_0 < 6; ++i_0)
    A += t43[1]*t33[1][i_0];
  return A;
}

int main()
{
  check_vect ();

  double w_1[18] = {0., 1.0, 1.0,
      0., 1.0, 1.0,
      0., 1.0, 1.0,
      0., 1.0, 1.0,
      0., 1.0, 1.0,
      0., 1.0, 1.0};
  double A = compute_integral(w_1);
  if (A != 12.0)
    __builtin_abort ();
  return 0;
}

/* { dg-final { scan-tree-dump-times {using an in-order \(fold-left\) reduction} 1 "vect" { target vect_double } } } */
/* { dg-final { scan-tree-dump-times "vectorized 2 loops" 1 "vect" { target { vect_double && { vect_perm && vect_hw_misalign } } } } } */
