/* PR tree-optimization/54713 */

#ifndef N
#define N 8
#define ONE 1, 1, 1, 1, 1, 1, 1, 1
#define ONEU 1U, 1U, 1U, 1U, 1U, 1U, 1U, 1U
#endif

typedef int V __attribute__((vector_size (N * sizeof (int))));
typedef unsigned int W __attribute__((vector_size (N * sizeof (int))));

void
f1 (V *p)
{
  *p = (*p & ((V) { ONE })) ^ ((V) { ONE});
}

void
f2 (V *p)
{
  *p = (*p ^ ((V) { ONE })) & ((V) { ONE});
}

void
f3 (V *p)
{
  *p = (~*p) & ((V) { ONE });
}

void
f4 (V *p, V *q)
{
  *p = (*p ^ *q) == *q;
}

void
f5 (V *p, V *q)
{
  *p = (*p ^ *q) == *p;
}

void
f6 (V *p, V *q, V *r)
{
  *p = (*p & *r) == (*q & *r);
}

void
f7 (V *p, V *q, V *r)
{
  *p = (*p & *r) == (*r & *q);
}

void
f8 (V *p, V *q, V *r)
{
  *p = (*r & *p) == (*q & *r);
}

void
f9 (V *p, V *q, V *r)
{
  *p = (*r & *p) == (*r & *q);
}

void
f10 (W *p, W *q)
{
  *p = *p < (((const W) { ONEU }) << *q);
}
