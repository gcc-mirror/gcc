/* { dg-do run } */
/* { dg-options "-O2" } */
/* { dg-options "-O2 -mcpu=i686" { target i?86-*-* } } */

extern void abort (void);
extern void exit (int);

struct A { char p[6]; } __attribute__((packed));
struct B {
    struct A a;
    void * const b;
    struct A const * const c;
    struct A const *d;
};

char v;

int __attribute__((noinline))
foo (struct B *b)
{
  int i;
  for (i = 0; i < 6; ++i)
    if (b->a.p[i])
      abort ();
  if (b->b != &v || b->c || b->d)
    abort ();
  return 12;
}

int __attribute__((noinline))
bar (void *x)
{
  __asm __volatile ("" : "=r" (x) : "0" (x));
  struct B y = { .b = x, .c = (void *) 0 };
  return foo (&y) + 1;
}

int
main (void)
{
  if (bar (&v) != 13)
    abort ();
  exit (0);
}
