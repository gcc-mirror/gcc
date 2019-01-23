/* PR tree-optimization/88800 - Spurious -Werror=array-bounds for non-taken
   branch
   { dg-do compile }
   { dg-options "-O2 -Wall" } */

extern void* memmove (void*, const void*, __SIZE_TYPE__);

struct A
{
  const char *s;
  int n;
};

void f (void*);

struct B
{
  char d[5];
  int n;
};

__attribute__ ((always_inline)) inline void
g (struct B *p, struct A a)
{
  int i = a.n;
  if (i <= 5)
    p->n = i;
  else {
    p->n = -1;
    f (p);
  }

  if (p->n >= 0)
    memmove (p->d, a.s, a.n);   /* { dg-bogus "\\\[-Warray-bounds" } */
}

void h (void)
{
  char c[8] = "";

  struct A a;
  a.s = c;
  a.n = 8;

  struct B b;
  g (&b, a);
}
