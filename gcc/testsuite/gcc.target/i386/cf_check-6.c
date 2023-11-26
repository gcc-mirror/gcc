/* { dg-do compile { target *-*-linux* *-*-gnu* } } */
/* { dg-options "-O2 -fhardened -mno-manual-endbr" } */
/* { dg-final { scan-assembler-times {\mendbr} 1 } } */
/* Test that -fhardened enables CET.  */

extern void bar (void) __attribute__((__cf_check__));

void
foo (void)
{
  bar ();
}
