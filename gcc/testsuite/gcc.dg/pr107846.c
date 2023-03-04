/* PR c/107846 */
/* { dg-do compile } */
/* { dg-options "-Wall -O2" } */

#define foo(x, b, n, m) ((unsigned short) (x) << (b - (n + 1) * 8) >> (b - 8) << (m * 8))
#define bar(x) ((unsigned short) (foo (x, 16, 0, 1) | foo (x, 16, 1, 0)))
#define baz(x)	bar (x)
static const int v = 8000;

unsigned short
qux (int t)
{
  return t != baz (v);
}
