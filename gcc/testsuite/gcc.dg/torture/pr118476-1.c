/* { dg-do compile } */

/* PR tree-optimization/118476 */

typedef unsigned long long poly64x1 __attribute__((__vector_size__(1*sizeof(long long))));

poly64x1 vext_p64(poly64x1 a, poly64x1 b, const int n)
{
  poly64x1 r = a;
  unsigned src = (unsigned)n;
  long long t = b[0];
  r[0] = (src < 1) ? a[src] : t;
  return r;
}
