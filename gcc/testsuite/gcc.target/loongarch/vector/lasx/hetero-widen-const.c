/* { dg-do compile } */
/* { dg-options "-O2 -mlasx" } */

#include <lasxintrin.h>

v4i64
test_xvaddwod_q_du_d (void)
{
  v4u64 a = { 0, 0, 0, 0 };
  v4i64 b = { -1, -1, -1, -1 };

  return __builtin_lasx_xvaddwod_q_du_d (a, b);
}
