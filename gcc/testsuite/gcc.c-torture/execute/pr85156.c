/* PR tree-optimization/85156 */

int x, y;

__attribute__((noipa)) int
foo (int z)
{
  if (__builtin_expect (x ? y != 0 : 0, z++))
    return 7;
  return z;
}

int
main ()
{
  x = 1;
  asm volatile ("" : "+m" (x), "+m" (y));
  if (foo (10) != 11)
    __builtin_abort ();
  return 0;
}
