/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

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

/* Verify the call clobbers all of a.  */

/* { dg-final { scan-tree-dump-not "return 1;" "optimized" } } */
