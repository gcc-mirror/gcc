/* { dg-do run } */
/* { dg-require-effective-target store_merge } */
/* { dg-options "-O2 -fdump-tree-store-merging" } */

/* Check that we can widen accesses to bitfields.  */

struct bar {
  int a : 3;
  unsigned char b : 4;
  unsigned char c : 1;
  char d;
  char e;
  char f;
  char g;
};

__attribute__ ((noinline)) void
foozero (struct bar *p)
{
  p->b = 0;
  p->a = 0;
  p->c = 0;
  p->d = 0;
  p->e = 0;
  p->f = 0;
  p->g = 0;
}

__attribute__ ((noinline)) void
foo1 (struct bar *p)
{
  p->b = 3;
  p->a = 2;
  p->c = 1;
  p->d = 4;
  p->e = 5;
}

int
main (void)
{
  struct bar p;
  foozero (&p);
  foo1 (&p);
  if (p.a != 2 || p.b != 3 || p.c != 1 || p.d != 4 || p.e != 5
      || p.f != 0 || p.g != 0)
    __builtin_abort ();

  return 0;
}


/* { dg-final { scan-tree-dump-times "Merging successful" 2 "store-merging" } } */
