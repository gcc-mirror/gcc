/* PR tree-optimization/51721 */
/* { dg-do compile } */
/* { dg-options "-O2 -Warray-bounds" } */

static int a[10], b[10], c[10], d[10];

unsigned int
f (unsigned int v)
{
  return v == 17 ? 11 : v;
}

unsigned int
g (unsigned int v)
{
  return v == 17 ? 17 : v;
}

void
t (unsigned int s)
{
  if (s >> 1 == 0)
    {
      a[f (s)] = 0;	/* { dg-bogus "array subscript is above array bounds" } */
      a[f (s)] = 0;	/* { dg-bogus "array subscript is above array bounds" } */
      b[f (s)] = 0;	/* { dg-bogus "array subscript is above array bounds" } */
      c[g (s)] = 0;	/* { dg-bogus "array subscript is above array bounds" } */
      c[g (s)] = 0;	/* { dg-bogus "array subscript is above array bounds" } */
      d[f (s)] = 0;	/* { dg-bogus "array subscript is above array bounds" } */
    }
}
