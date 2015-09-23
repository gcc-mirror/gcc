// { dg-do compile }
// { dg-options "-O2 -fdump-tree-optimized" }

struct S { int *__restrict p; int q; };
S s;

int
f1 (S x, S y)
{
  x.p[0] = 1;
  y.p[0] = 0;
// { dg-final { scan-tree-dump-times "return 1" 1 "optimized" } }
  return x.p[0];
}

int
f2 (S x)
{
  x.p[0] = 2;
  s.p[0] = 0;
// { dg-final { scan-tree-dump-times "return 2" 1 "optimized" } }
  return x.p[0];
}

int
f3 (S &__restrict x, S &__restrict y)
{
  x.p[0] = 3;
  y.p[0] = 0;
// { dg-final { scan-tree-dump-times "return 3" 1 "optimized" } }
  return x.p[0];
}

int
f4 (S &x, S &y)
{
  x.p[0] = 4;
  y.p[0] = 0;
// { dg-final { scan-tree-dump-times "return 4" 0 "optimized" } }
  return x.p[0];
}

int
f5 (S *__restrict x, S *__restrict y)
{
  x->p[0] = 5;
  y->p[0] = 0;
// { dg-final { scan-tree-dump-times "return 5" 1 "optimized" } }
  return x->p[0];
}

int
f6 (S *x, S *y)
{
  x->p[0] = 6;
  y->p[0] = 0;
// { dg-final { scan-tree-dump-times "return 6" 0 "optimized" } }
  return x->p[0];
}

