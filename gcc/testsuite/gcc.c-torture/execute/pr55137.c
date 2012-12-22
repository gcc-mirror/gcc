/* PR c++/55137 */

extern void abort (void);

int
foo (unsigned int x)
{
  return ((int) (x + 1U) + 1) < (int) x;
}

int
bar (unsigned int x)
{
  return (int) (x + 1U) + 1;
}

int
baz (unsigned int x)
{
  return x + 1U;
}

int
main ()
{
  if (foo (__INT_MAX__) != (bar (__INT_MAX__) < __INT_MAX__)
      || foo (__INT_MAX__) != ((int) baz (__INT_MAX__) + 1 < __INT_MAX__))
    abort ();
  return 0;
}
