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
/* { dg-final { scan-tree-dump-not "offset: (4294967292|0x0f+fc)" "ivopts" } } */
/* { dg-final { scan-tree-dump-not "&x\\\[5\\\]" "ivopts" } } */
/* { dg-final { cleanup-tree-dump "ivopts" } } */
