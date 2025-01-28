/* PR rtl-optimization/118638 */

__attribute__((noipa)) int
foo (int x)
{
  int a = x != -3, b, c;
  a *= 3;
  b = 2 * x - 9;
  a = a + b;
  a = ~a;
  c = a & 1;
  return -c;
}

int
main ()
{
  if (foo (0) != -1)
    __builtin_abort ();
}
