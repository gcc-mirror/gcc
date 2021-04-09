/* PR tree-optimization/98727 */

__attribute__((noipa)) long int
foo (long int x, long int y)
{
  long int z = (unsigned long) x * y;
  if (x != z / y)
    return -1;
  return z;
}

int
main ()
{
  if (foo (4, 24) != 96
      || foo (124, 126) != 124L * 126
      || foo (__LONG_MAX__ / 16, 17) != -1)
    __builtin_abort ();
  return 0;
}
