/* { dg-do compile } */
/* { dg-options "-O2 -fno-inline" } */

#include <arm_neon.h>

void
foo1 (uint64_t *a)
{
  uint64x1_t val18;
  uint32x2_t val19;
  uint64x1_t val20;
  val19 = vcreate_u32 (0x800000004cf3dffbUL);
  val20 = vrsra_n_u64 (val18, vreinterpret_u64_u32 (val19), 34);
  vst1_u64 (a, val20);
}

void
foo2 (uint64_t *a)
{
  uint64x1_t val18;
  uint32x2_t val19;
  uint64x1_t val20;
  val19 = vcreate_u32 (0xdffbUL);
  val20 = vrsra_n_u64 (val18, vreinterpret_u64_u32 (val19), 34);
  vst1_u64 (a, val20);
}
