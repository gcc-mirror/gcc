/* { dg-do compile } */
/* { dg-additional-options "-fdump-tree-slp-details" } */

struct x { double d[2]; };

struct x
pack (double a, double aa)
{
  struct x u;
  u.d[0] = a;
  u.d[1] = aa;
  return u;
}

/* { dg-final { scan-tree-dump-times "vectorization is not profitable" 1 "slp2" } } */
