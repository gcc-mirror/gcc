/* PR tree-optimization/94524 */

typedef signed char __attribute__ ((__vector_size__ (16))) V;

static __attribute__ ((__noinline__, __noclone__)) V
foo (V c)
{
  c %= (signed char) -128;
  return (V) c;
}

int
main ()
{
  V x = foo ((V) { -128 });
  if (x[0] != 0)
    __builtin_abort ();
  x = foo ((V) { -127 });
  if (x[0] != -127)
    __builtin_abort ();
  x = foo ((V) { 127 });
  if (x[0] != 127)
    __builtin_abort ();
  return 0;
}
