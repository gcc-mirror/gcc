/* { dg-do compile } */
/* { dg-options "-O2 -fcf-protection=none" } */
/* { dg-final { scan-assembler-not {\mendbr} } } */

extern void bar (void) __attribute__((__cf_check__));

void
foo (void)
{
  bar ();
}
