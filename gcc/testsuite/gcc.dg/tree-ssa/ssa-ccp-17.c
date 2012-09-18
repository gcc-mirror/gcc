/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-ccp1" } */

int foo(void)
{
  int i = 0;
  char *p = (char *)&i;
  return *(int *)p;
}

struct Foo {
  int i;
} f;

int bar(void)
{
  char *p = (char *)&f;
  return ((struct Foo *)p)->i;
}

extern const struct Foo g;

int foobar(void)
{
  struct Foo *p = (struct Foo *)&g;
  return ((const struct Foo *)p)->i;
}

/* { dg-final { scan-tree-dump "= i_.;" "ccp1" } } */
/* { dg-final { scan-tree-dump "= f.i;" "ccp1" } } */
/* { dg-final { scan-tree-dump "= g.i;" "ccp1" } } */
/* { dg-final { cleanup-tree-dump "ccp1" } } */
