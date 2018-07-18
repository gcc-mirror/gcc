/* PR tree-optimization/82434 */
/* { dg-do compile } */
/* { dg-require-effective-target store_merge } */
/* { dg-options "-O2 -fdump-tree-store-merging" } */

enum E { E0, E1, E2 = __INT_MAX__, E3 = -__INT_MAX__ - 1 };

struct bar {
  enum E a;
  char b;
  _Bool c;
  short d;
};

void
foo1 (struct bar *p)
{
  p->b = 0;
  p->a = E0;
  p->c = (_Bool) 0;
  p->d = 0;
}

void
foo2 (struct bar *p)
{
  p->b = 0;
  p->a = E0;
  p->c = (_Bool) 1;
  p->d = 0;
}

/* { dg-final { scan-tree-dump-times "Merging successful" 2 "store-merging" } } */
