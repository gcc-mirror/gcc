/* { dg-do compile } */

int n;

void
bar (int, int);

__attribute__ ((noinline, returns_twice)) int
zero (void)
{
  return 0;
}

void
foo (void)
{
  (void) zero ();

  n = 0;

  for (;;)
    bar (zero (), n);
}
