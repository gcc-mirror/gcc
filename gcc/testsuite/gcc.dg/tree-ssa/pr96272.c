/* PR tree-optimization/96272 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-widening_mul" } */

unsigned
foo (unsigned a, unsigned b)
{
  if (a > ~0U - b)
    return ~0U;
  return a + b;
}

unsigned
bar (unsigned a, unsigned b)
{
  if (a <= ~0U - b)
    return ~0U;
  return a + b;
}

unsigned
baz (unsigned a, unsigned b)
{
  if (~0U - b < a)
    return ~0U;
  return a + b;
}

unsigned
qux (unsigned a, unsigned b)
{
  if (~0U - b >= a)
    return ~0U;
  return a + b;
}

/* { dg-final { scan-tree-dump-times "ADD_OVERFLOW" 4 "widening_mul" { target { i?86-*-* x86_64-*-* } } } } */
