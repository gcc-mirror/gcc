/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized"  } */
struct a
{
  int b;
  int c;
};

__attribute__ ((noclone, noinline))
void
test (struct a *a)
{
  a->b = 2;
}
int
foo ()
{
  struct a a = {113,114};
  test (&a);
  return a.c;
}
int
foo2 (struct a *a)
{
  a->b = 123;
  a->c = 124;
  test (a);
  return a->c;
}
/* { dg-final { scan-tree-dump "return 114" "optimized"} } */
/* { dg-final { scan-tree-dump "return 124" "optimized"} } */
