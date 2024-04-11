/* Test BTF generation for non-representable bitfields.

   Due to the limitations of BTF, we only have 24 bits in which to store
   the bitfield offset (in bits, from the beginning of the struct).

   In this test, we construct a structure such that the bitfield will have
   an offset so large as to be unrepresentable in BTF. We expect that the
   resulting BTF will describe the rest of the structure, ignoring the
   non-representable bitfield by simply using void data type for the same.  */

/* { dg-do compile } */
/* { dg-options "-O0 -gbtf -dA" } */
/* { dg-require-effective-target size32plus } */

/* Struct with 4 members and no bitfield (kind_flag not set).  */
/* { dg-final { scan-assembler-times "\[\t \]0x4000004\[\t \]+\[^\n\]*btt_info" 1 } } */
/* { dg-final { scan-assembler-times " MEMBER" 4 } } */
/* { dg-final { scan-assembler-times " MEMBER 'unsup' idx=2\[\\r\\n\]+\[^\\r\\n\]*0\[\t \]+\[^\n\]*btm_type: void" 1 } } */

struct bigly
{
  int a;
  int b[((0xffffff + 1) / (8 * sizeof (int)))];
  unsigned unsup : 7;
  char c;
} big;
