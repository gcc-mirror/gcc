/* Test BTF generation for struct with 0 size bitfield.

   We expect a struct with 2 members to be generated. The size 0 bitfield
   should not have any entry in the member list.  */

/* { dg-do compile )  */
/* { dg-options "-O0 -gbtf -dA" } */

/* Struct with bitfield members, and 2 members.  */
/* { dg-final { scan-assembler-times "\[\t \]0x84000002\[\t \]+\[^\n\]*btt_info" 1 } } */

/* Bitfield size 31 (0x1f) at offset 0.  */
/* { dg-final { scan-assembler-times "\[\t \]0x1f000000\[\t \]+\[^\n\]*btm_offset" 1 } } */

/* Bitfield size 32 (0x20) at offset 32.  */
/* { dg-final { scan-assembler-times "\[\t \]0x20000020\[\t \]+\[^\n\]*btm_offset" 1 } } */

/* Only 2 members.  */
/* { dg-final { scan-assembler-times "btm_name" 2 } } */

struct foo
{
  unsigned a : 31;
  unsigned   : 0;
  unsigned c : 32;
} myfoo;
