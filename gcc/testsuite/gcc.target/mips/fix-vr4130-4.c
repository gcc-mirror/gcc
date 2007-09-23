/* { dg-do compile } */
/* { dg-mips-options "-march=vr4130 -mgp64 -mfix-vr4130" } */
NOMIPS16 long long
foo (void)
{
  long long r;
  asm ("# foo" : "=l" (r));
  return r;
}
/* { dg-final { scan-assembler "\tdmacc\t" } } */
