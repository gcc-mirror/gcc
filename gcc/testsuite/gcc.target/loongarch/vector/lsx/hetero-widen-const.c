/* { dg-do compile } */
/* { dg-options "-O2 -mlsx" } */

#include <lsxintrin.h>

v2i64
test_vaddwod_q_du_d (void)
{
  v2u64 a = { 0, 0 };
  v2i64 b = { -1, -1 };

  return __builtin_lsx_vaddwod_q_du_d (a, b);
}
