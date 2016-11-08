/* { dg-do compile } */
/* { dg-require-effective-target store_merge } */
/* { dg-options "-O2 -fdump-tree-store-merging" } */

/* Check that we can merge interleaving stores that are guaranteed
   to be non-aliasing.  */

struct bar
{
  int a;
  char b;
  char c;
  short d;
  char e;
  char f;
  char g;
};

void
foozero (struct bar *restrict p, struct bar *restrict p2)
{
  p->b = 0xff;
  p2->b = 0xa;
  p->a = 0xfffff;
  p2->a = 0xab;
  p2->c = 0xc;
  p->c = 0xff;
  p2->d = 0xbf;
  p->d = 0xfff;
}

/* { dg-final { scan-tree-dump-times "Merging successful" 2 "store-merging" } } */
