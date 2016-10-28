/* { dg-do compile } */
/* { dg-require-effective-target non_strict_align } */
/* { dg-options "-O2 -fdump-tree-store-merging-details" } */

/* Make sure stores to volatile addresses don't get combined with
   other accesses.  */

struct bar
{
  int a;
  char b;
  char c;
  volatile short d;
  char e;
  char f;
  char g;
};

void
foozero (struct bar *p)
{
  p->b = 0xa;
  p->a = 0xb;
  p->c = 0xc;
  p->d = 0;
  p->e = 0xd;
  p->f = 0xe;
  p->g = 0xf;
}

/* { dg-final { scan-tree-dump "Volatile access terminates all chains" "store-merging" } } */
/* { dg-final { scan-tree-dump-times "=\{v\} 0;" 1 "store-merging" } } */
