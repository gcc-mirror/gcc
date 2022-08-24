/* { dg-options "-mgeneral-regs-only" } */
/* { dg-excess-errors "arm_neon.h" } */

#include <arm_neon.h>

int32x4x4_t
test (int32_t *ptr) /* { dg-error "-mgeneral-regs-only" } */
{
  return vld4q_s32 (ptr);
}
