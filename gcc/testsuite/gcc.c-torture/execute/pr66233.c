/* PR tree-optimization/66233 */

unsigned int v[8];

__attribute__((noinline, noclone)) void
foo (void)
{
  int i;
  for (i = 0; i < 8; i++)
    v[i] = (float) i;
}

int
main ()
{
  unsigned int i;
  foo ();
  for (i = 0; i < 8; i++)
    if (v[i] != i)
      __builtin_abort ();
  return 0;
}
