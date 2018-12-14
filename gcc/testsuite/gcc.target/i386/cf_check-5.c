/* { dg-do compile } */
/* { dg-options "-O2 -fcf-protection -mmanual-endbr" } */
/* { dg-final { scan-assembler-times {\mendbr} 1 } } */

__attribute__((__cf_check__))
void
foo (void)
{
}
