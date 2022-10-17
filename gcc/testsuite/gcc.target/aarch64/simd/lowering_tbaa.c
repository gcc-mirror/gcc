/* Tests the TBAA information of lowered AArch64 SIMD loads.  */
/* { dg-do run } */
/* { dg-options "-save-temps -O2" } */

#include <arm_neon.h>

void __attribute__((noipa))
g (float *)
{
}

int32x4_t __attribute__((noipa))
f (void)
{
  float a[4] = { 1, 2, 3, 4 };
  g (a);
  a[0] = a[1] = a[2] = a[3] = 0;
  void *volatile ptr = a;
  return vld1q_s32 ((int32_t *) ptr);
}

int
main (void)
{
  int32x4_t x = f ();
  int32x4_t y = vdupq_n_s32 (0);
  if (__builtin_memcmp (&x, &y, 16) != 0)
    __builtin_abort ();
  return 0;
}
