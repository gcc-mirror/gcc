/* { dg-do compile } */
// { dg-options "-g -dA" }
// { dg-final { scan-assembler-times "DW_AT_name: \"svbool_t\"" 1 } }
// { dg-final { scan-assembler-times "DW_AT_name: \"svcount_t\"" 1 } }
// { dg-final { scan-assembler-times ".byte	0x1	// DW_AT_bit_stride" 1 } }
// { dg-final { scan-assembler-times ".byte	0x1	// DW_AT_bit_size" 1 } }

#include <arm_sme.h>

#pragma GCC target "+sve2p1+sme2"

void fun ()
{
  volatile svbool_t pred;
  volatile svcount_t count;
}
