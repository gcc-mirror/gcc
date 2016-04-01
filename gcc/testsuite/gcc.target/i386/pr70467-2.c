/* PR rtl-optimization/70467 */
/* { dg-do compile { target ia32 } } */
/* { dg-options "-O2" } */

unsigned long long
foo (unsigned long long x)
{
  return x + 0x12345600000000ULL;
}

unsigned long long
bar (unsigned long long x)
{
  return x - 0x12345600000000ULL;
}

/* { dg-final { scan-assembler-not "addl\[ \t\]*.0," } } */
/* { dg-final { scan-assembler-not "subl\[ \t\]*.0," } } */
/* { dg-final { scan-assembler-not "adcl\[^\n\r\]*%" } } */
/* { dg-final { scan-assembler-not "sbbl\[^\n\r\]*%" } } */
