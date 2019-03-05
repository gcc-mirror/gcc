/* { dg-do compile } */
/* { dg-options "-O2 -fcf-protection -mno-manual-endbr" } */
/* { dg-final { scan-assembler-times {\mendbr} 1 } } */

extern void bar (void) __attribute__((__cf_check__));

void
foo (void)
{
  bar ();
}
