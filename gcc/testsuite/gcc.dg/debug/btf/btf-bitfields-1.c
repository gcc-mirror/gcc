/* Basic tests for BTF bitfields.

   The structure containing bitfield members should be marked with KIND_FLAG=1
   The bitfield member offsets should be encoded as:
     (bit_size << 24) | bit_offset
     - (0xa  << 24) | 0x20
     - (0x7  << 24) | 0x2a
     - (0x13 << 24) | 0x40 - note that this is aligned to 0x40.  */

/* { dg-do compile )  */
/* { dg-options "-O0 -gbtf -dA" } */

/* { dg-final { scan-assembler-times "\[\t \]0x84000004\[\t \]+\[^\n\]*btt_info" 1 } } */

/* { dg-final { scan-assembler-times "\[\t \]0xa000020\[\t \]+\[^\n\]*btm_offset" 1 } } */
/* { dg-final { scan-assembler-times "\[\t \]0x700002a\[\t \]+\[^\n\]*btm_offset" 1 } } */
/* { dg-final { scan-assembler-times "\[\t \]0x13000040\[\t \]+\[^\n\]*btm_offset" 1 } } */

struct bitt {
  int a;
  unsigned int bitfield_a : 10;
  unsigned int bitfield_b : 7;
  unsigned int bitfield_c : 19;
} bitty;

struct no_bitt {
  int a;
  int b;
} no_bitty;

int main ()
{
  return bitty.bitfield_b + bitty.a;
}
