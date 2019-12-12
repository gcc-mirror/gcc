/* { dg-do compile { target *-*-linux* } } */
/* { dg-options "-O2 -fno-pic -fcf-protection -pg -fasynchronous-unwind-tables" } */
/* { dg-final { scan-assembler-times {\t\.cfi_startproc\n\tendbr} 1 } } */

extern int bar (int);

int
foo (int i)
{
  return bar (i);
}
