/* { dg-do compile } */
/* { dg-options "-mabi=lp64d -O0 -fpic -fplt -mexplicit-relocs -mcmodel=normal" } */
/* { dg-final { scan-assembler "test:.*bl\t%plt\\(g\\)\n" } } */
/* { dg-final { scan-assembler "test1:.*bl\t%plt\\(f\\)\n" } } */
/* { dg-final { scan-assembler "test2:.*bl\tl\n" } } */

extern void g (void);

void
f (void)
{}

static void
l (void)
{}

void
test (void)
{
  g ();
}

void
test1 (void)
{
  f ();
}

void
test2 (void)
{
  l ();
}
