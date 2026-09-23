/* PR tree-optimization/126601 */
/* { dg-do compile } */
/* { dg-options "-Os" } */

volatile int c[16];

[[gnu::noipa]] int
foo (unsigned x, unsigned y)
{
  unsigned r = x * y;
  int t = 0;
  if (c[0]) { int u = 0; if (x != 0) u = (r / x != y); t += u; }
  if (c[1]) { int u = 0; if (x != 0) u = (r / x != y); t += u; }
  if (c[2]) { int u = 0; if (x != 0) u = (r / x != y); t += u; }
  if (c[3]) { int u = 0; if (x != 0) u = (r / x != y); t += u; }
  if (c[4]) { int u = 0; if (x != 0) u = (r / x != y); t += u; }
  if (c[5]) { int u = 0; if (x != 0) u = (r / x != y); t += u; }
  if (c[6]) { int u = 0; if (x != 0) u = (r / x != y); t += u; }
  if (c[7]) { int u = 0; if (x != 0) u = (r / x != y); t += u; }
  if (c[8]) { int u = 0; if (x != 0) u = (r / x != y); t += u; }
  if (c[9]) { int u = 0; if (x != 0) u = (r / x != y); t += u; }
  if (c[10]) { int u = 0; if (x != 0) u = (r / x != y); t += u; }
  if (c[11]) { int u = 0; if (x != 0) u = (r / x != y); t += u; }
  if (c[12]) { int u = 0; if (x != 0) u = (r / x != y); t += u; }
  if (c[13]) { int u = 0; if (x != 0) u = (r / x != y); t += u; }
  if (c[14]) { int u = 0; if (x != 0) u = (r / x != y); t += u; }
  if (c[15]) { int u = 0; if (x != 0) u = (r / x != y); t += u; }
  return t;
}
