/* { dg-do run } */
/* { dg-options "-O2 -fipa-pta" } */

struct Foo {
  int *p;
  int *q;
};

void __attribute__((noinline))
bar (int **x)
{
  struct Foo *f = (struct Foo *)(x - 1);
  *(f->p) = 0;
}

int foo(void)
{
  struct Foo f;
  int i = 1, j = 2;
  f.p = &i;
  f.q = &j;
  bar(&f.q);
  return i;
}

extern void abort (void);
int main()
{
  if (foo () != 0)
    abort ();
  return 0;
}
