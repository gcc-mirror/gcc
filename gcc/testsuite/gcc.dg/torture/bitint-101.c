/* PR tree-optimization/126257 */
/* { dg-do run { target bitint } } */

_BitInt(2) a;
long long b, c, d;

int
main ()
{
  int e;
  _BitInt(2) f;
  bool g;
  long long h = b;
lab:
  e = h;
  if (e != 4)
    f = -1;
  f = f + f;
  a = f;
  g = d - 4;
  if (!g)
    goto lab;
  c = a;
  if (c != -2)
    __builtin_abort ();
}
