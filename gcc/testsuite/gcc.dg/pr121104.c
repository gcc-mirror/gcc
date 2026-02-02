/* PR tree-optimization/121104 */
/* { dg-do compile } */
/* { dg-options "-O2" } */

static unsigned long
foo (unsigned long x, unsigned long y,
     unsigned long z, unsigned long *w)
{
  int r;
  unsigned long a = __builtin_sub_overflow (x, y, &r);
  unsigned long b = __builtin_sub_overflow (r, z, &r);
  *w = a + b;
  return r;
}

unsigned long
bar (unsigned long *p, unsigned long *q)
{
  unsigned long c;
  p[0] = foo (p[0], q[0], 0, &c);
  p[1] = foo (p[1], q[1], c, &c);
  return c;
}
