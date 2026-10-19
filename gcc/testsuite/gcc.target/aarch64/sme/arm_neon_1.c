// { dg-options "" }

#include <arm_neon.h>

#pragma GCC target "+nosme"

int32x4_t
foo (int32x4_t x, int32x4_t y) [[arm::streaming_compatible]]
{
  return vhaddq_s32 (x, y); // { dg-error {ACLE function 'vhaddq_s32' cannot be called when SME streaming mode is enabled} }
}
