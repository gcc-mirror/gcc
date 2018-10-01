/* { dg-do compile { target *-*-linux* } } */
/* { dg-options "-O2 -fcf-protection -mfentry -fasynchronous-unwind-tables" } */
/* { dg-additional-options "-fno-pic" { target ia32 } } */
/* { dg-final { scan-assembler-times {\t\.cfi_startproc\n\tendbr} 1 } } */

extern int bar (int);

int
foo (int i)
{
  return bar (i);
}
