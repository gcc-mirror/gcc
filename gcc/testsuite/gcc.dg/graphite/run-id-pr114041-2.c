/* PR tree-optimization/114041 */
/* { dg-options "-O -fgraphite-identity" } */

unsigned a[24], b[24];
enum E { E0 = 0, E1 = 1, E42 = 42, E56 = 56 };

__attribute__((noipa)) unsigned
foo (enum E x)
{
  for (int i = 0; i < 24; ++i)
    a[i] = i;
  unsigned e;
  if (x >= E42)
    e = __builtin_clz ((unsigned) x);
  else
    e = 42;
  for (int i = 0; i < 24; ++i)
    b[i] = i;
  return e;
}

int
main ()
{
  if (foo (E1) != 42 || foo (E56) != __SIZEOF_INT__ * __CHAR_BIT__ - 6)
    __builtin_abort ();
}
