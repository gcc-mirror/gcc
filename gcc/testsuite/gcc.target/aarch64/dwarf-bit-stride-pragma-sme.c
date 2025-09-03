/* { dg-do compile } */
// { dg-options "-g -dA" }
// { dg-final { scan-assembler-times "DW_AT_name: \"__SVBool_t\"" 2 } }
// { dg-final { scan-assembler-times "DW_AT_name: \"__SVCount_t\"" 1 } }
// { dg-final { scan-assembler-times ".byte	0x1	// DW_AT_bit_stride" 2 } }
// { dg-final { scan-assembler-times ".byte	0x1	// DW_AT_bit_size" 1 } }

#pragma GCC target "+sve2p1+sme2"

void fun ()
{
  volatile __SVBool_t pred;
  volatile __SVCount_t count;
}
