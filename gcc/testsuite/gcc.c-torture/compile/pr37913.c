/* PR middle-end/37913 */

void foo (void) __attribute__ ((noreturn));

static int __attribute__ ((noreturn))
bar (void)
{
  foo ();
}

void
baz (void)
{
  int i = bar ();
}
