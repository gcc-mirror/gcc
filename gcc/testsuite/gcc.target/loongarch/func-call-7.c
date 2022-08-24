/* { dg-do compile } */
/* { dg-options "-mabi=lp64d -O0 -fpic -fno-plt -mexplicit-relocs -mcmodel=normal" } */
/* { dg-final { scan-assembler "test:.*pcalau12i\t.*%got_pc_hi20\\(g\\)\n\tld\.d\t.*%got_pc_lo12\\(g\\)\n\tjirl" } } */
/* { dg-final { scan-assembler "test1:.*pcalau12i\t.*%got_pc_hi20\\(f\\)\n\tld\.d\t.*%got_pc_lo12\\(f\\)\n\tjirl" } } */
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
