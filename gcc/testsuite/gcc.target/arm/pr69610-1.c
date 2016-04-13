/* Check that pre ARMv4 compilation still works.  */
/* { dg-do compile } */
/* { dg-options "-marm -march=armv3 -ftree-ter" } */
/* { dg-require-effective-target arm_arm_ok } */

typedef unsigned short v16u16 __attribute__ ((vector_size (16)));
typedef unsigned int v16u32 __attribute__ ((vector_size (16)));

unsigned short
foo (v16u16 v16u16_1, v16u32 v16u32_1)
{
  v16u16_1 += (v16u16) v16u32_1;
  return v16u16_1[5] + v16u32_1[1];
}
