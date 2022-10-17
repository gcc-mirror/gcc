/* { dg-lto-do link } */
/* { dg-lto-options { { -flto -O1 } } } */

double m;
int n;

__attribute__ ((optimize ("-funsafe-math-optimizations")))
void
bar (int x)
{
  n = x;
  m = n;
}

__attribute__ ((flatten))
void
foo (int x)
{
  bar (x);
}

void
quux (void)
{
  ++n;
}

int
main (void)
{
  foo (0);
  quux ();

  return 0;
}
