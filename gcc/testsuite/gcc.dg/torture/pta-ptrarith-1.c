/* { dg-do run } */
/* { dg-options "-fdump-tree-alias-details" } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-fno-fat-lto-objects" } { "" } } */

struct Foo {
  int *p;
};

struct Foo *ff;

void __attribute__((noinline))
foo (void *p)
{
  struct Foo *f = (struct Foo *)p - 1;
  *f->p = 0;
  ff = f;
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

/* { dg-final { scan-tree-dump "ESCAPED = {\[^\n\}\]* i f \[^\n\}\]*}" "alias" } } */
