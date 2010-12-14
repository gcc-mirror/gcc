/* PR tree-optimization/46909 */

extern void abort ();

int
__attribute__ ((__noinline__))
foo (unsigned int x)
{
  if (! (x == 4 || x == 6) || (x == 2 || x == 6))
    return 1;
  return -1;
}

int
main ()
{
  int i;
  for (i = -10; i < 10; i++)
    if (foo (i) != 1 - 2 * (i == 4))
      abort ();
  return 0;
}
