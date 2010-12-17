/* { dg-lto-do link } */
/* { dg-lto-options {{-flto -r -nostdlib -O}} } */

extern void baz (void);

static void __attribute__ ((constructor))
bar (void)
{
  baz ();
}

void
foo (void)
{
  bar ();
}
