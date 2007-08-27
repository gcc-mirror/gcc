/* { dg-do compile { target mips64*-*-* mipsisa64*-*-* } } */
/* { dg-options "-O2 -mlong64" } */
int foo (int *x, int i)
{
  return x[i] + i;
}
/* { dg-final { scan-assembler-not "move" } } */
