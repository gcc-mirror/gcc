/* PR rtl-optimization/64682 */

int a, b = 1;

__attribute__((noinline, noclone)) void
foo (int x)
{
  if (x != 5)
    __builtin_abort ();
}

int
main ()
{
  int i;
  for (i = 0; i < 56; i++)
    for (; a; a--)
      ;
  int *c = &b;
  if (*c)
    *c = 1 % (unsigned int) *c | 5;

  foo (b);

  return 0;
}
