/* { dg-do compile } */
/* { dg-require-effective-target lp64 } */
/* { dg-options "-O2 -fcf-protection -mcmodel=large" } */
/* { dg-final { scan-assembler-times {\mendbr} 2 } } */

extern void ext (void);

__attribute((noclone, noinline))
static
void
foo (void)
{
  ext ();
}

void
bar (void)
{
  foo ();
}
