/* PR tree-optimization/67830 */
/* { dg-do run } */
/* { dg-options "-O2" } */

int a, b, *g, h;
unsigned char c, d;

int
main ()
{
  int f, e = -2;
  b = e;
  g = &b;
  h = c = a + 1;
  f = d - h;
  *g &= f;

  if (b != -2)
    __builtin_abort ();

  return 0;
}
