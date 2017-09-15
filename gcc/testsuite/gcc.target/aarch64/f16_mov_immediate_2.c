/* { dg-do compile } */
/* { dg-options "-O3" } */
/* { dg-require-effective-target arm_v8_2a_fp16_scalar_ok } */
/* { dg-add-options arm_v8_2a_fp16_scalar } */

#include <arm_fp16.h>

float16_t f0(void)
{
  float16_t x = 0.0f;
  return x;
}

float16_t fn1(void)
{
  float16_t x = -0.0f;
  return x;
}

float16_t f1(void)
{
  float16_t x = 256.0f;
  return x;
}

float16_t f2(void)
{
  float16_t x = 123256.0f;
  return x;
}

float16_t f3(void)
{
  float16_t x = 17.0;
  return x;
}

/* { dg-final { scan-assembler-times "movi\tv\[0-9\]+\\\.4h, ?#0"         1 } } */
/* { dg-final { scan-assembler-times "movi\tv\[0-9\]+\\\.2s, 0x80, lsl 8" 1 } } */
/* { dg-final { scan-assembler-times "movi\tv\[0-9\]+\\\.2s, 0x5c, lsl 8" 1 } } */
/* { dg-final { scan-assembler-times "movi\tv\[0-9\]+\\\.2s, 0x7c, lsl 8" 1 } } */

/* { dg-final { scan-assembler-times "mov\tw\[0-9\]+, 19520"              1 } } */
/* { dg-final { scan-assembler-times "fmov\th\[0-9\], w\[0-9\]+"          1 } } */

