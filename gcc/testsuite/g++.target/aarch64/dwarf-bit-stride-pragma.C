/* { dg-do compile } */
// { dg-options "-g -dA" }
// { dg-final { scan-assembler-times "DW_AT_name: \"svbool_t\"" 1 } }
// { dg-final { scan-assembler-times ".byte	0x1	// DW_AT_bit_stride" 1 } }
// { dg-final { scan-assembler-times ".byte	0x1	// DW_AT_bit_size" 1 } }

#include <arm_sve.h>

#pragma GCC target "+sve"

void fun ()
{
  volatile svbool_t pred8 = svwhilelt_b8_u32 (0u, 1u);
  volatile svbool_t pred16 = svwhilelt_b16_u32 (0u, 3u);
  volatile svbool_t pred32 = svwhilelt_b32_u32 (0u, 7u);
  volatile svbool_t pred64 = svwhilelt_b64_u32 (0u, 11u);
}
