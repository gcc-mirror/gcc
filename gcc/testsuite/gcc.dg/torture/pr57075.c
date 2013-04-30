/* { dg-do compile } */

extern int baz (void) __attribute__ ((returns_twice));
int __attribute__ ((__leaf__))
foo (void)
{
  return __builtin_printf ("$");
}

void
bar ()
{
  foo ();
  baz ();
}
