/* PR tree-optimization/69209 */

int a, c, *d, e;

void foo (void) __attribute__ ((__noreturn__));

int
bar (void)
{
  int f;
  if (a)
    {
      if (e)
	foo ();
      foo ();
    }
  if (d != &f)
    foo ();
  if (!c)
    foo ();
  return f;
}

void
baz ()
{
  bar ();
}
