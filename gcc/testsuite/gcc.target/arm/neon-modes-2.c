/* { dg-do compile } */
/* { dg-require-effective-target arm_neon_ok } */
/* { dg-options "-O1" } */
/* { dg-add-options arm_neon } */

#include "arm_neon.h"

#define SETUP(A) x##A = vld3_u32 (ptr + A * 0x20)
#define MODIFY(A) x##A = vld3_lane_u32 (ptr + A * 0x20 + 0x10, x##A, 1)
#define STORE(A) vst3_u32 (ptr + A * 0x20, x##A)

#define MANY(A) A (0), A (1), A (2), A (3), A (4), A (5)

extern void foo (int *, int *);

void
bar (uint32_t *ptr, int y)
{
  uint32x2x3_t MANY (SETUP);
  int *x = __builtin_alloca (y);
  int z[0x1000];
  foo (x, z);
  MANY (MODIFY);
  foo (x, z);
  MANY (STORE);
}
