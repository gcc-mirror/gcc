/* { dg-do compile { target { ia32 && { ! *-*-darwin* } } } } */
/* { dg-options "-msse2 -mgeneral-regs-only" } */

extern float a, b, c;

void
foo (void)
{
  c = a * b;
}

/* { dg-final { scan-assembler-not "mulss" } } */
/* { dg-final { scan-assembler "call\[ \t\]__mulsf3" } } */
