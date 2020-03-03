/* PR tree-optimization/93582 */

short a;
int b, c;

__attribute__((noipa)) void
foo (void)
{
  b = c;
  a &= 7;
}

int
main ()
{
  c = 27;
  a = 14;
  foo ();
  if (b != 27 || a != 6)
    __builtin_abort ();
  return 0;
}
