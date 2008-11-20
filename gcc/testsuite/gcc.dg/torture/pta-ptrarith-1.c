/* { dg-do run } */

struct Foo {
  int *p;
};

void __attribute__((noinline))
foo (void *p)
{
  struct Foo *f = (struct Foo *)p - 1;
  *f->p = 0;
}

int bar (void)
{
  struct Foo f;
  int i = 1;
  f.p = &i;
  foo (&f + 1);
  return i;
}
extern void abort (void);
int main()
{
  if (bar () != 0)
    abort ();
  return 0;
}

