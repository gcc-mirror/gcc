/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-alias1-vops" } */

typedef struct {
  int i;
  int j;
  int k;
} Foo;

void bar(Foo*);
int foo(void)
{
  Foo a;
  a.i = 1;
  bar(&a);
  return a.i;
}

/* { dg-final { scan-tree-dump-times "V_MAY_DEF" 2 "alias1" } } */
/* { dg-final { cleanup-tree-dump "alias1" } } */
