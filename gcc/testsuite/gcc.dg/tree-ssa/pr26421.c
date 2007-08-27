/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-salias-vops" } */

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

/* { dg-final { scan-tree-dump-times "VDEF" 4 "salias" } } */
/* { dg-final { cleanup-tree-dump "salias" } } */
