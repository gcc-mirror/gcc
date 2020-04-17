/* { dg-do compile } */
/* { dg-options "-O2 -fcf-protection -mforce-indirect-call" } */
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
