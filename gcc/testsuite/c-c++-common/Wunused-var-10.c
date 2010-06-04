/* PR c++/44362 */
/* { dg-options "-Wunused" } */
/* { dg-do compile } */

int
f1 (int u, int v)
{
  int a, b, c, d, e, f, g, h, i;
  a = u;
  b = v;
  c = u;
  d = v;
  e = u;
  f = v;
  g = u == 6 ? a : b;
  h = 0 ? c : d;
  i = 1 ? e : f;
  return g + h + i;
}

int
f2 (int u, int v)
{
  int a, b, c, d, e, f, g, h, i;
  a = u;
  b = v;
  c = u;
  d = v;
  e = u;
  f = v;
  g = u == 6 ? a + 1 : b;
  h = 0 ? c + 1 : d;
  i = 1 ? e + 1 : f;
  return g + h + i;
}

int
f3 (int u, int v)
{
  int a, b, c, d, e, f, g, h, i;
  a = u;
  b = v;
  c = u;
  d = v;
  e = u;
  f = v;
  g = u == 6 ? a : b + 1;
  h = 0 ? c : d + 1;
  i = 1 ? e : f + 1;
  return g + h + i;
}

int
f4 (int u, int v)
{
  int a, c, e, g, h, i;
  long b, d, f;
  a = u;
  b = v;
  c = u;
  d = v;
  e = u;
  f = v;
  g = u == 6 ? a : b;
  h = 0 ? c : d;
  i = 1 ? e : f;
  return g + h + i;
}
