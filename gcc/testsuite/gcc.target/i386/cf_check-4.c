/* { dg-do compile } */
/* { dg-options "-O2 -fcf-protection -mmanual-endbr" } */
/* { dg-final { scan-assembler-times {\mendbr} 1 } } */

extern void foo (void) __attribute__((__cf_check__));

void
foo (void)
{
}
