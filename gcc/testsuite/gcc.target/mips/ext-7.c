/* No need to use ext if we can use andi.  */
/* { dg-do compile } */
/* { dg-options "-O isa_rev>=2" } */
/* { dg-final { scan-assembler "\tandi\t" } } */
/* { dg-final { scan-assembler-not "\td?ext\t" } } */

NOMIPS16 unsigned
f (unsigned i)
{
  return i & 0x7fff;
}
