/* PR middle-end/101950 */
/* { dg-do compile } */
/* { dg-options "-O2 -mlzcnt" } */
/* { dg-final { scan-assembler-not "call\[^\n\r]*__clrsb.i2" } } */
/* { dg-final { scan-assembler-times "\tlzcnt\[ql]\t" 2 } } */
/* { dg-final { scan-assembler-times "\txor\[ql]\t" 2 } } */
/* { dg-final { scan-assembler-times "\tsar\[ql]\t|\tcltd" 2 } } */

int
foo (long x)
{
  return __builtin_clrsbl (x);
}

int
bar (int x)
{
  return __builtin_clrsb (x);
}
