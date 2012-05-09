/* PR rtl-optimization/53160 */

extern void abort (void);

int a, c = 1, d, e, g;
volatile int b;
volatile char f;
long h;
short i;

void
foo (void)
{
  for (e = 0; e; ++e)
    ;
}

int
main ()
{
  if (g)
    (void) b;
  foo ();
  for (d = 0; d >= 0; d--)
    {
      short j = f;
      int k = 0;
      i = j ? j : j << k;
    }
  h = c == 0 ? 0 : i;
  a = h;
  if (a != 0)
    abort ();
  return 0;
}
