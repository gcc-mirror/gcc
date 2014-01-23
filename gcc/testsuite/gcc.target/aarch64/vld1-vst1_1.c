/* Test vld1 and vst1 maintain consistent indexing.  */
/* { dg-do run } */
/* { dg-options "-O3" } */
#include <arm_neon.h>

extern void abort (void);

int __attribute__ ((noinline))
test_vld1_vst1 ()
{
  int8x8_t a;
  int8x8_t b;
  int i = 0;
  int8_t c[8] = { 0, 1, 2, 3, 4, 5, 6, 7 };
  int8_t d[8];
  a = vld1_s8 (c);
  asm volatile ("":::"memory");
  vst1_s8 (d, a);
  asm volatile ("":::"memory");
  for (; i < 8; i++)
    if (c[i] != d[i])
      return 1;
  return 0;
}

int __attribute__ ((noinline))
test_vld1q_vst1q ()
{
  int16x8_t a;
  int16x8_t b;
  int i = 0;
  int16_t c[8] = { 0, 1, 2, 3, 4, 5, 6, 7 };
  int16_t d[8];
  a = vld1q_s16 (c);
  asm volatile ("":::"memory");
  vst1q_s16 (d, a);
  asm volatile ("":::"memory");
  for (; i < 8; i++)
    if (c[i] != d[i])
      return 1;
  return 0;
}

int
main ()
{
  if (test_vld1_vst1 ())
    abort ();
  if (test_vld1q_vst1q ())
    abort ();
  return 0;
}
