/* PR rtl-optimization/119002 */

__attribute__((noipa)) unsigned int
foo (void *x, float y, float z)
{
  unsigned int a, b;
  float c, d, e;
  c = y;
  d = z;
  a = c < d;
  d = y;
  e = z;
  b = d >= e;
  a |= b;
  return a;
}

int
main ()
{
  if (foo ((void *) 0, 0.f, __builtin_nanf ("")))
    __builtin_abort ();
}
