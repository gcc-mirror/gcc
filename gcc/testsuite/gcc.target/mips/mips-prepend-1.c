/* { dg-options "-mdspr2" } */
/* { dg-final { scan-assembler "prepend\[^\n\]*,10" } } */

NOMIPS16 int
foo (int x, int y)
{
  return __builtin_mips_prepend (x, y, 42);
}
