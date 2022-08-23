/* { dg-do compile } */
/* { dg-options "-O2 -march=x86-64" } */

void
foo (int *c)
{
  c = __builtin_assume_aligned (c, 16);
  c[0] = -1;
  c[1] = -1;
}

/* { dg-final { scan-assembler-times "movq\[ \\t\]+\[^\n\]*%xmm" 2 { target { ia32 } } } } */
/* { dg-final { scan-assembler-times "movq\[ \\t\]+\\\$-1," 1 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-not "xmm" { target { ! ia32 } } } } */
