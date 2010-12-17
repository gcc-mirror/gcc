/* PR tree-optimization/46099 */
/* { dg-do compile } */
/* { dg-options "-ftree-parallelize-loops=2 -fcompare-debug -O" } */

static inline void
bar (int *i)
{
  int j = *i;
}

void baz (int *, int *, int *);

void
f1 (int n)
{
  int i;
  for (i = 0; i < n; i++)
    bar (&i);
}

void
f2 (int n)
{
  int i;
  int a[10000], b[10000], c[10000];
  baz (a, b, c);
  for (i = 0; i < n; i++)
    {
      void *p = c;
      a[i] = b[i] + c[i];
    }
  baz (a, b, c);
}

void
f3 (int n)
{
  int i;
  int a[10000], b[10000], c[10000];
  baz (a, b, c);
  for (i = 0; i < n; i++)
    {
      a[i] = b[i] + c[i];
      void *p = c;
    }
  baz (a, b, c);
}
