/* { dg-require-effective-target label_values } */

void fn (void);

void
foo (void *x, unsigned long y)
{
  asm goto ("": : : : lab);
lab:
  fn ();
}

static void
bar (unsigned long x)
{
  foo (0, x);
}

static void
baz (unsigned long x)
{
  if (x > 8192)
    bar (x);
  else
    ({ __here: (unsigned long) &&__here; });
}

void
test (void)
{
  baz (16384);
}
