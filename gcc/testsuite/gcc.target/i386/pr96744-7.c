/* { dg-do compile { target { ia32 && { ! *-*-darwin* } } } } */
/* { dg-options "-msse2" } */

extern float a, b, c;

__attribute__((__target__("general-regs-only")))
void
foo (void)
{
  c = a * b;
}

/* { dg-final { scan-assembler-not "mulss" } } */
/* { dg-final { scan-assembler "call\[ \t\]__mulsf3" } } */
