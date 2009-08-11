/* Check the shift_shift alternative of the AND patterns.  */
/* { dg-do compile } */
/* { dg-options "-O isa_rev<=1 -mgp64" } */
/* { dg-final { scan-assembler "\tdsrl\t" } } */
/* { dg-final { scan-assembler "\tdsll\t" } } */
/* { dg-final { scan-assembler-not "\td?ext\t" } } */

unsigned long long
f (unsigned long long i)
{
  return i & 0xffffffff;
}
