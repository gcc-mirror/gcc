/* PR tree-optimization/65053 */

int i;

__attribute__ ((noinline, noclone))
unsigned int foo (void)
{
  return 0;
}

int
main ()
{
  unsigned int u = -1;
  if (u == -1)
    {
      unsigned int n = foo ();
      if (n > 0)
	u = n - 1;
    }

  while (u != -1)
    {
      asm ("" : "+g" (u));
      u = -1;
      i = 1;
    }

  if (i)
    __builtin_abort ();
  return 0;
}
