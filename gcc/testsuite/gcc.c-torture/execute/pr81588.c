/* PR tree-optimization/81588 */

__attribute__((noinline, noclone)) int
bar (int x)
{
  __asm volatile ("" : : "g" (x) : "memory");
}

__attribute__((noinline, noclone)) int
foo (unsigned x, long long y)
{
  if (y < 0)
    return 0;
  if (y < (long long) (4 * x))
    {
      bar (y);
      return 1;
    }
  return 0;
}     

int
main ()
{
  volatile unsigned x = 10;
  volatile long long y = -10000;
  if (foo (x, y) != 0)
    __builtin_abort ();
  y = -1;
  if (foo (x, y) != 0)
    __builtin_abort ();
  y = 0;
  if (foo (x, y) != 1)
    __builtin_abort ();
  y = 39;
  if (foo (x, y) != 1)
    __builtin_abort ();
  y = 40;
  if (foo (x, y) != 0)
    __builtin_abort ();
  y = 10000;
  if (foo (x, y) != 0)
    __builtin_abort ();
  return 0;
}
