/* Also make sure we don't use ext for MIPS*r1.  */
/* { dg-do compile } */
/* { dg-options "isa_rev<=1" } */
/* { dg-final { scan-assembler "\tand\t" } } */
/* { dg-final { scan-assembler-not "\td?ext\t" } } */

unsigned
f (unsigned i)
{
  return i & 0x7fffff;
}
