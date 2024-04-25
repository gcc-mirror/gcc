/* Check add and sub instructions.  */
/* { dg-do compile } */
/* { dg-options "-masm=normal" } */

long foo (long x, long y)
{
  return y - x + 4;
}

/* { dg-final { scan-assembler-not {sub\t(%r.),\1\n} } } */
/* { dg-final { scan-assembler {sub\t(\%r.),(\%r.)\n} } } */
/* { dg-final { scan-assembler {add\t(\%r.),4\n} } } */
