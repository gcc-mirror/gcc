/* PR target/78791 */

__attribute__((used, noinline, noclone)) unsigned long long
foo (unsigned long long x, unsigned long long y, unsigned long long z)
{
  unsigned long long a = x / y;
  unsigned long long b = x % y;
  a |= z;
  b ^= z;
  return a + b;
}

int
main ()
{
  if (foo (64, 7, 0) != 10 || foo (28, 3, 2) != 14)
    __builtin_abort ();
  return 0;
}
