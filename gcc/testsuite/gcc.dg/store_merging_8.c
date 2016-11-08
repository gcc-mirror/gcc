/* { dg-do compile } */
/* { dg-require-effective-target store_merge } */
/* { dg-options "-O2 -fdump-tree-store-merging" } */

struct baz {
  struct bar {
      int a;
      char b;
      char c;
      char d;
      char e;
      char f;
      char g;
  } a[4];
} x;
struct baz *xx = &x;

void
foo1 (int i)
{
  x.a[i].b = 0;
  x.a[i].a = 0;
  x.a[i].c = 0;
  x.a[i].d = 0;
  x.a[i].e = 0;
}

void
foo2 (int i)
{
  x.a[i].b = 0;
  x.a[i].a = 0;
  x.a[i].c = 1;
  x.a[i].d = 0;
  x.a[i].e = 0;
}

/* { dg-final { scan-tree-dump-times "Merging successful" 2 "store-merging" } } */
