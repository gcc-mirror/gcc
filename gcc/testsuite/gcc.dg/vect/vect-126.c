/* PR tree-optimization/66718 */
/* { dg-do compile } */
/* { dg-additional-options "-fdump-tree-optimized-details-blocks" } */

int *a[1024], b[1024];
struct S { int u, v, w, x; };
struct S c[1024];
int d[1024][10];

void
f0 (void)
{
  for (int i = 0; i < 1024; i++)
    a[i] = &b[0];
}

void
f1 (void)
{
  for (int i = 0; i < 1024; i++)
    {
      int *p = &b[0];
      a[i] = p + i;
    }
}

void
f2 (int *p)
{
  for (int i = 0; i < 1024; i++)
    a[i] = &p[i];
}

void
f3 (void)
{
  for (int i = 0; i < 1024; i++)
    a[i] = &b[i];
}

void
f4 (void)
{
  int *p = &c[0].v;
  for (int i = 0; i < 1024; i++)
    a[i] = &p[4 * i];
}

void
f5 (void)
{
  for (int i = 0; i < 1024; i++)
    a[i] = &c[i].v;
}

void
f6 (void)
{
  for (int i = 0; i < 1024; i++)
    for (unsigned int j = 0; j < 10; j++)
      a[i] = &d[i][j];
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops in function" 7 "vect" { target { i?86-*-* x86_64-*-* } } } } */
/* { dg-final { scan-tree-dump-not "Invalid sum" "optimized" } } */
