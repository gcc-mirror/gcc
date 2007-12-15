/* { dg-do compile { target cris-*-* crisv32-*-* } } */
/* { dg-skip-if "" { cris*-*-* } { "-march*" } { "" } } */
/* { dg-options "-O2 -march=v10" } */
/* { dg-final { scan-assembler "in-asm: .mof" } } */
/* { dg-final { scan-assembler "out-asm: .mof" } } */
/* { dg-final { scan-assembler "in2-asm: .mof" } } */
/* { dg-final { scan-assembler "out2-asm: .mof" } } */

unsigned int
in (unsigned int i)
{
  register int i0 asm ("mof") = i;
  asm ("in-asm: %0" : : "x" (i0));
}

unsigned int
out (void)
{
  register int o asm ("mof");
  asm ("out-asm: %0" : "=x" (o));
  return o;
}

unsigned int
in2 (unsigned int i)
{
  asm ("in2-asm: %0" : : "h" (i));
}

unsigned int
out2 (void)
{
  unsigned int o;
  asm ("out2-asm: %0" : "=h" (o));
  return o;
}
