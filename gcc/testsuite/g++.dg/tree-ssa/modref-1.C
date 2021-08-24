/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
struct S { int a; char b[20]; S(); S(S const&); };
volatile int global;

__attribute__ ((noinline,noclone))
struct S noescape (int *b)
{
  struct S a;
  a.a = b!=0;
  global = 1;
  return a;
}

void escape (struct S *p);

__attribute__ ((noinline,noclone))
int
test(int *b)
{
  struct S s = noescape (b);
  escape (&s);
  return *b;
}
int test2()
{
  int b=1234;
  test (&b);
  return b;
}
// ipa-modref should analyze parameter B of test as noescape.
// { dg-final { scan-tree-dump "return 1234" "optimized" } }
