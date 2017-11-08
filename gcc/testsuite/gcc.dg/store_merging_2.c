/* { dg-do run } */
/* { dg-require-effective-target store_merge } */
/* { dg-options "-O2 -fdump-tree-store-merging" } */

struct bar
{
  int a;
  unsigned char b;
  unsigned char c;
  short d;
  unsigned char e;
  unsigned char f;
  unsigned char g;
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
  p->b = 1;
  p->a = 2;
  p->c = 3;
  p->d = 4;
  p->e = 5;
  p->f = 0;
  p->g = 0xff;
}

__attribute__ ((noinline)) void
foo2 (struct bar *p, struct bar *p2)
{
  p->b = 0xff;
  p2->b = 0xa;
  p->a = 0xfffff;
  p2->c = 0xc;
  p->c = 0xff;
  p2->d = 0xbf;
  p->d = 0xfff;
}

int
main (void)
{
  struct bar b1, b2;
  foozero (&b1);
  foozero (&b2);

  foo1 (&b1);
  if (b1.b != 1 || b1.a != 2 || b1.c != 3 || b1.d != 4 || b1.e != 5
      || b1.f != 0 || b1.g != 0xff)
    __builtin_abort ();

  foozero (&b1);
  /* Make sure writes to aliasing struct pointers preserve the
     correct order.  */
  foo2 (&b1, &b1);
  if (b1.b != 0xa || b1.a != 0xfffff || b1.c != 0xff || b1.d != 0xfff)
    __builtin_abort ();

  foozero (&b1);
  foo2 (&b1, &b2);
  if (b1.a != 0xfffff || b1.b != 0xff || b1.c != 0xff || b1.d != 0xfff
      || b2.b != 0xa || b2.c != 0xc || b2.d != 0xbf)
    __builtin_abort ();

  return 0;
}

/* { dg-final { scan-tree-dump-times "Merging successful" 3 "store-merging" } } */
