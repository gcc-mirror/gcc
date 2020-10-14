/* PR target/97387 */
/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -fomit-frame-pointer" } */
/* { dg-final { scan-assembler-times "\taddq\t" 1 } } */
/* { dg-final { scan-assembler-times "\tadcq\t" 3 } } */
/* { dg-final { scan-assembler-times "\tsubq\t" 1 } } */
/* { dg-final { scan-assembler-times "\tsbbq\t" 3 } } */
/* { dg-final { scan-assembler-not "\tset\[bc]\t" } } */
/* { dg-final { scan-assembler-not "\taddb\t" } } */

#include <x86intrin.h>

void
foo (unsigned long long a[4], unsigned long long b[4])
{
  unsigned char carry = 0;
  carry = _addcarry_u64 (carry, a[0], b[0], &a[0]);
  carry = _addcarry_u64 (carry, a[1], b[1], &a[1]);
  carry = _addcarry_u64 (carry, a[2], b[2], &a[2]);
  _addcarry_u64 (carry, a[3], b[3], &a[3]);
}

void
bar (unsigned long long a[4], unsigned long long b[4])
{
  unsigned char carry = 0;
  carry = _subborrow_u64 (carry, a[0], b[0], &a[0]);
  carry = _subborrow_u64 (carry, a[1], b[1], &a[1]);
  carry = _subborrow_u64 (carry, a[2], b[2], &a[2]);
  _subborrow_u64 (carry, a[3], b[3], &a[3]);
}
