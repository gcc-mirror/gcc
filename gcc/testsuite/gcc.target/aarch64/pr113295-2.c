// { dg-do run }
// { dg-options "-O2" }

#include <arm_neon.h>

void __attribute__ ((noinline))
foo (int8_t **ptr)
{
  int8x16_t v0 = vld1q_s8 (ptr[0]);
  int8x16_t v1 = vld1q_s8 (ptr[1]);
  int8x16_t v2 = vld1q_s8 (ptr[2]);
  int8x16_t v3 = vld1q_s8 (ptr[3]);
  int8x16_t v4 = vld1q_s8 (ptr[4]);

  int8x16x4_t res0 = { v0, v1, v2, v3 };
  vst4q_s8 (ptr[5], res0);

  int8x16_t add = vaddq_s8 (v2, v3);
  int8x16x3_t res1 = { v1, add, v3 };
  vst3q_s8 (ptr[6], res1);

  int8x16x3_t res2 = { v0, v1, v2 };
  vst3q_s8 (ptr[7], res2);
}

int8_t arr0[16] = { 1 };
int8_t arr1[16] = { 2 };
int8_t arr2[16] = { 4 };
int8_t arr3[16] = { 8 };
int8_t arr4[16] = { 16 };
int8_t arr5[16 * 4];
int8_t arr6[16 * 3];
int8_t arr7[16 * 3];
int8_t *ptr[] =
{
  arr0,
  arr1,
  arr2,
  arr3,
  arr4,
  arr5,
  arr6,
  arr7
};

int
main (void)
{
  foo (ptr);
  if (arr5[0] != 1 || arr5[1] != 2 || arr5[2] != 4 || arr5[3] != 8)
    __builtin_abort ();
  if (arr6[0] != 2 || arr6[1] != 12 || arr6[2] != 8)
    __builtin_abort ();
  if (arr7[0] != 1 || arr7[1] != 2 || arr7[2] != 4)
    __builtin_abort ();
  return 0;
}
