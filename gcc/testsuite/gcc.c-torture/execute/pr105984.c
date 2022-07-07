/* PR tree-optimization/105984 */

unsigned long long g;

static inline unsigned long long
foo (unsigned char c)
{
  g -= __builtin_mul_overflow_p (4, (unsigned char) ~c, 0);
  return g;
}

int
main ()
{
  unsigned long long x = foo (1);
  if (x != 0)
    __builtin_abort ();
  return 0;
}
