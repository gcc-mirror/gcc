/* { dg-do compile } */
/* { dg-options "-O" } */

#include <arm_neon.h>

int16x8_t
test_16x8 (uint16x8_t a)
{
  return vclsq_u16 (a);
}

/* { dg-final { scan-assembler-times "cls\\tv\[0-9\]+\.8h, v\[0-9\]+\.8h" 1 } } */


int8x16_t
test_8x16 (uint8x16_t a)
{
  return vclsq_u8 (a);
}

/* { dg-final { scan-assembler-times "cls\\tv\[0-9\]+\.16b, v\[0-9\]+\.16b" 1 } } */

int32x4_t
test_32x4 (uint32x4_t a)
{
  return vclsq_u32 (a);
}

/* { dg-final { scan-assembler-times "cls\\tv\[0-9\]+\.4s, v\[0-9\]+\.4s" 1 } } */

int16x4_t
test_16x4 (uint16x4_t a)
{
  return vcls_u16 (a);
}

/* { dg-final { scan-assembler-times "cls\\tv\[0-9\]+\.4h, v\[0-9\]+\.4h" 1 } } */

int8x8_t
test_8x8 (uint8x8_t a)
{
  return vcls_u8 (a);
}

/* { dg-final { scan-assembler-times "cls\\tv\[0-9\]+\.8b, v\[0-9\]+\.8b" 1 } } */

int32x2_t
test32x2 (uint32x2_t a)
{
  return vcls_u32 (a);
}

/* { dg-final { scan-assembler-times "cls\\tv\[0-9\]+\.2s, v\[0-9\]+\.2s" 1 } } */

