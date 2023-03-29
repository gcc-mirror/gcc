/* { dg-do compile } */
/* { dg-options "-O2 -fno-guess-branch-probability" } */

int n;

void
bar (void);

__attribute__ ((noinline, returns_twice)) int
zero (void)
{
  return 0;
}

void
foo (void)
{
  int a = zero ();

  for (n = 0; n < 2; ++n)
    {
    }

  if (a)
    bar ();
}
