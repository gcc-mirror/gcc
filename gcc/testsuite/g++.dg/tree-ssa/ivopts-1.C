/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-ivopts" } */

struct Foo {
  Foo() : s(1) {}
  int s;
};
void foo(Foo&);
void bar(void)
{
  Foo x[4];
  foo(x[0]);
}

/* { dg-final { scan-tree-dump-not "-&x" "ivopts" } } */
/* { dg-final { scan-tree-dump-not "offset: -4B" "ivopts" { xfail i?86-*-* x86_64-*-* hppa*-*-* } } } */
/* { dg-final { scan-tree-dump-not "&x\\\[5\\\]" "ivopts" { xfail i?86-*-* x86_64-*-* hppa*-*-* } } } */
/* { dg-final { cleanup-tree-dump "ivopts" } } */
