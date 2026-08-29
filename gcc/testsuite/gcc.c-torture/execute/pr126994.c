/* PR tree-optimization/126994 */

int a, b;

__attribute__((noinline, noclone)) void
foo (void)
{
  long c = 0;
  int d = 0;
  for (; a < 2; a++)
    do
      {
	d++;
	c ^= 3;
      }
    while (d < 2);
  b = c;
}

int
main (void)
{
  foo ();
  if (b != 3)
    __builtin_abort ();
  return 0;
}
