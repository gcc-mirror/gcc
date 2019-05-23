/* { dg-do compile { target *-*-linux* } } */
/* { dg-require-effective-target mfentry } */
/* { dg-options "-fno-pic -O2 -fcf-protection -pg -mfentry -mrecord-mcount -mnop-mcount -fasynchronous-unwind-tables" } */
/* { dg-final { scan-assembler-times {\t\.cfi_startproc\n\tendbr} 1 } } */

extern int bar (int);

int
foo (int i)
{
  return bar (i);
}
