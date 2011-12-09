/* PR tree-optimization/50078 */
/* { dg-do compile } */
/* { dg-options "-O2" } */

unsigned nonvolvar[2];

void
test (int arg)
{
  unsigned v = *(volatile unsigned *) (&nonvolvar[arg]);
  *(volatile unsigned *) (&nonvolvar[arg]) = v;
}

/* { dg-final { scan-assembler-times "movl\[^\n\r\]*nonvolvar" 2 { target { { i?86-*-* x86_64-*-* } && nonpic } } } } */
