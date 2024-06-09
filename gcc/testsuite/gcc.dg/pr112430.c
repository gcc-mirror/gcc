/* PR middle-end/112430 */
/* { dg-do compile } */
/* { dg-options "-O2 -g" } */

int a, b, c, d, e;
unsigned int f;

static void
foo (unsigned int x)
{
  unsigned int g = x < c;
  int h = f < b;
  x += h;
  g += x < h;
  f = x;
  x = g;
  g = f += a;
  h = f < a;
  x += h;
  c += f < d;
  x += c;
  g += x < c;
  e = g;
}

void
bar (unsigned int x)
{
  foo (x);
}
