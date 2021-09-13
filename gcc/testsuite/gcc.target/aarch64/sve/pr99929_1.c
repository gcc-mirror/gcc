/* { dg-do run { target aarch64_sve_hw } } */
/* { dg-options "-O2 -ftree-vectorize" } */

#include <arm_sve.h>

static void e(short *g, short p2) { *g ^= p2; }
static short m[23];
int main() {
  for (unsigned i = 0; i < 23; ++i)
    m[i] = 4;
  if (svaddv(svptrue_pat_b32(SV_VL1), svdup_u32(1)) != 1)
    __builtin_abort();
  for (unsigned i = 0; i < 3; ++i)
    e(m, m[i]);
  return 0;
}
