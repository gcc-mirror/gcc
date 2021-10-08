/* { dg-do compile } */
/* { dg-require-effective-target store_merge } */
/* { dg-options "-O2 -fno-tree-vectorize -fdump-tree-store-merging" } */

struct bar {
  int a;
  char b;
  char c;
  char d;
  char e;
  char f;
  char g;
};

void
foo1 (struct bar *p)
{
  p->b = 0;
  p->a = 0;
  p->c = 0;
  p->d = 0;
  p->e = 0;
}

void
foo2 (struct bar *p)
{
  p->b = 0;
  p->a = 0;
  p->c = 1;
  p->d = 0;
  p->e = 0;
}

/* { dg-final { scan-tree-dump-times "Merging successful" 2 "store-merging" } } */
