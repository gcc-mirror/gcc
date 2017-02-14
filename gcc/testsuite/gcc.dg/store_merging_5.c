/* { dg-do compile } */
/* { dg-require-effective-target store_merge } */
/* { dg-options "-O2 -fdump-tree-store-merging" } */

/* Make sure that non-aliasing non-constant interspersed stores do not
   stop chains.  */

struct bar {
  int a;
  char b;
  char c;
  char d;
  char e;
  char g;
};

void
foo1 (struct bar *p, char tmp)
{
  p->a = 0;
  p->b = 0;
  p->g = tmp;
  p->c = 0;
  p->d = 0;
  p->e = 0;
}


/* { dg-final { scan-tree-dump-times "Merging successful" 1 "store-merging" } } */
/* { dg-final { scan-tree-dump-times "MEM\\\[.*\\\]" 1 "store-merging" } } */
