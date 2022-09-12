/* { dg-do compile } */
/* { dg-options "-O1 -fno-ipa-pure-const -Wuninitialized" } */

int n;

void
empty (int)
{
}

int
bar (int x)
{
  return n + x + 1;
}

__attribute__ ((pure, returns_twice)) int
foo (void)
{
  int uninitialized;

  if (n)
    {
      if (bar (0))
        return 0;

      __builtin_unreachable ();
    }

  while (uninitialized < 1) /* { dg-warning "uninitialized" } */
    {
      foo ();
      empty (bar (0) == foo ());
      ++uninitialized;
    }

  return 0;
}
