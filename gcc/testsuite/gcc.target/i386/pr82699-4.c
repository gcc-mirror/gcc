/* { dg-do compile { target { *-*-linux* && { ! ia32 } } } } */
/* { dg-require-effective-target mfentry } */
/* { dg-options "-O2 -fpic -fcf-protection -pg -mfentry -fasynchronous-unwind-tables" } */
/* { dg-final { scan-assembler-times {\t\.cfi_startproc\n\tendbr} 1 } } */

extern int bar (int);

int
foo (int i)
{
  return bar (i);
}
