/* Check that pre ARMv4 compilation still works.  */
/* { dg-do compile } */
/* { dg-options "-marm -march=armv3 -O2 -fno-forward-propagate" } */
/* { dg-require-effective-target arm_arm_ok } */

typedef short v16u16 __attribute__ ((vector_size (16)));
typedef unsigned v16u32 __attribute__ ((vector_size (16)));
typedef long long v16u64 __attribute__ ((vector_size (16)));

unsigned
foo
  (int
   u16_0,
   unsigned
   u32_0,
   int
   u64_0,
   int
   u16_1,
   unsigned
   u64_1,
   v16u16
   v16u16_0,
   v16u32
   v16u32_0,
   v16u64 v16u64_0, v16u16 v16u16_1, v16u32 v16u32_1, v16u64 v16u64_1)
{
  v16u16_1[3] -= v16u32_0[0];
  v16u16_0 -= (v16u16) v16u32_0;
  return u16_0 + u32_0 + u64_0 + u16_1 +
        v16u16_0[0] + v16u16_0[2] + v16u16_0[3] + v16u16_0[4] + v16u16_0[5] + v16u32_0[0] + v16u32_0[1] + v16u32_0[3] + v16u64_0[1] +
        v16u16_1[2] + v16u16_1[3] + v16u16_1[5] + v16u16_1[7] + v16u32_1[0] + v16u32_1[3] + v16u64_1[0] + v16u64_1[1];
}
