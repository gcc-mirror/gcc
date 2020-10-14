/* PR target/97387 */
/* { dg-do compile } */
/* { dg-options "-O2 -fomit-frame-pointer" } */
/* { dg-final { scan-assembler-times "\taddl\t" 1 } } */
/* { dg-final { scan-assembler-times "\tadcl\t" 3 } } */
/* { dg-final { scan-assembler-times "\tsubl\t" 1 } } */
/* { dg-final { scan-assembler-times "\tsbbl\t" 3 } } */
/* { dg-final { scan-assembler-not "\tset\[bc]\t" } } */
/* { dg-final { scan-assembler-not "\taddb\t" } } */

#include <x86intrin.h>

void
foo (unsigned int a[4], unsigned int b[4])
{
  unsigned char carry = 0;
  carry = _addcarry_u32 (carry, a[0], b[0], &a[0]);
  carry = _addcarry_u32 (carry, a[1], b[1], &a[1]);
  carry = _addcarry_u32 (carry, a[2], b[2], &a[2]);
  _addcarry_u32 (carry, a[3], b[3], &a[3]);
}

void
bar (unsigned int a[4], unsigned int b[4])
{
  unsigned char carry = 0;
  carry = _subborrow_u32 (carry, a[0], b[0], &a[0]);
  carry = _subborrow_u32 (carry, a[1], b[1], &a[1]);
  carry = _subborrow_u32 (carry, a[2], b[2], &a[2]);
  _subborrow_u32 (carry, a[3], b[3], &a[3]);
}
