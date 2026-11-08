/* PR tree-optimization/127904 */
/* { dg-do run { target bitint575 } } */

typedef unsigned _BitInt(575) B;

[[gnu::noipa]] B
foo (B a, B b, int c, unsigned n)
{
  B r = 0;
  for (unsigned i = 0; i < n; i++)
    {
      B x;
      if (c)
       x = a;
      else
       x = b;
      for (unsigned j = 0; j < n; j++)
       x = (B) j;
      r += x;
    }
  return r + a + b;
}

int
main ()
{
  if (foo (5, 7, 0, 2) != 14 || foo (5, 7, 1, 3) != 18)
    __builtin_abort ();
}
