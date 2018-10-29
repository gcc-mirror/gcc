/* { dg-lto-do link } */
/* { dg-lto-options {{-flto -r -O}} } */
/* { dg-extra-ld-options "-flinker-output=nolto-rel" } */

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
