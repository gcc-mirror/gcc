/* PR tree-optimization/53410 */

typedef int V __attribute__((vector_size (4 * sizeof (int))));
typedef unsigned int W __attribute__((vector_size (4 * sizeof (int))));

void
f1 (V *p)
{
  *p = (*p & ((V) { 1, 1, 1, 1 })) ^ ((V) { 1, 1, 1, 1});
}

void
f2 (V *p)
{
  *p = (*p ^ ((V) { 1, 1, 1, 1 })) & ((V) { 1, 1, 1, 1});
}

void
f3 (V *p)
{
  *p = (~*p) & ((V) { 1, 1, 1, 1 });
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
  *p = *p < (((const W) { 1U, 1U, 1U, 1U }) << *q);
}
