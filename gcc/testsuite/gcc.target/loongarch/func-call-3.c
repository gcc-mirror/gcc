/* { dg-do compile } */
/* { dg-options "-mabi=lp64d -O0 -fpic -fno-plt -mno-explicit-relocs -mcmodel=normal" } */
/* { dg-final { scan-assembler "test:.*la\.global\t.*g\n\tjirl" } } */
/* { dg-final { scan-assembler "test1:.*la\.global\t.*f\n\tjirl" } } */
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
