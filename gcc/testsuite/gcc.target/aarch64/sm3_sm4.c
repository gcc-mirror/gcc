/* { dg-do compile } */
/* { dg-options "-march=armv8.4-a+sm4" } */

#include "arm_neon.h"

uint32x4_t
test_vsm3ss1q_u32 (uint32x4_t a, uint32x4_t b, uint32x4_t c)
{
  return vsm3ss1q_u32 (a, b, c);
}

/* { dg-final { scan-assembler-times "sm3ss1\\tv\[0-9\]+\.4s, v\[0-9\]+\.4s, v\[0-9\]+\.4s, v\[0-9\]+\.4s" 1 } } */

uint32x4_t
test_vsm3tt1aq_u32 (uint32x4_t a, uint32x4_t b, uint32x4_t c)
{
  return vsm3tt1aq_u32 (a, b, c, 3);
}

/* { dg-final { scan-assembler-times "sm3tt1a\\tv\[0-9\]+\.4s, v\[0-9\]+\.4s, v\[0-9\]+\.4s\\\[3\\\]" 1 } } */

uint32x4_t
test_vsm3tt1bq_u32 (uint32x4_t a, uint32x4_t b, uint32x4_t c)
{
  return vsm3tt1bq_u32 (a, b, c, 1);
}

/* { dg-final { scan-assembler-times "sm3tt1b\\tv\[0-9\]+\.4s, v\[0-9\]+\.4s, v\[0-9\]+\.4s\\\[1\\\]" 1 } } */

uint32x4_t
test_vsm3tt2aq_u32 (uint32x4_t a, uint32x4_t b, uint32x4_t c)
{
  return vsm3tt2aq_u32 (a, b, c, 2);
}

/* { dg-final { scan-assembler-times "sm3tt2a\\tv\[0-9\]+\.4s, v\[0-9\]+\.4s, v\[0-9\]+\.4s\\\[2\\\]" 1 } } */

uint32x4_t
test_vsm3tt2bq_u32 (uint32x4_t a, uint32x4_t b, uint32x4_t c)
{
  return vsm3tt2bq_u32 (a, b, c, 3);
}

/* { dg-final { scan-assembler-times "sm3tt2b\\tv\[0-9\]+\.4s, v\[0-9\]+\.4s, v\[0-9\]+\.4s\\\[3\\\]" 1 } } */

uint32x4_t
test_vsm3partw1q_u32 (uint32x4_t a, uint32x4_t b, uint32x4_t c)
{
  return vsm3partw1q_u32 (a, b, c);
}

/* { dg-final { scan-assembler-times "sm3partw1\\tv\[0-9\]+\.4s, v\[0-9\]+\.4s, v\[0-9\]+\.4s" 1 } } */

uint32x4_t
test_vsm3partw2q_u32 (uint32x4_t a, uint32x4_t b, uint32x4_t c)
{
  return vsm3partw2q_u32 (a, b, c);
}

/* { dg-final { scan-assembler-times "sm3partw2\\tv\[0-9\]+\.4s, v\[0-9\]+\.4s, v\[0-9\]+\.4s" 1 } } */

// SM4

uint32x4_t
test_vsm4eq_u32 (uint32x4_t a, uint32x4_t b)
{
  return vsm4eq_u32 (a, b);
}

/* { dg-final { scan-assembler-times "sm4e\\tv\[0-9\]+\.4s, v\[0-9\]+\.4s" 1 } } */

uint32x4_t
test_vsm4ekeyq_u32 (uint32x4_t a, uint32x4_t b)
{
  return vsm4ekeyq_u32 (a, b);
}

/* { dg-final { scan-assembler-times "sm4ekey\\tv\[0-9\]+\.4s, v\[0-9\]+\.4s, v\[0-9\]+\.4s" 1 } } */
