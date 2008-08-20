/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-forwprop1" } */

int b;
unsigned a;

static inline int *g(void)
{
  a = 1;
  return (int*)&a;
}
void test2(void)
{
  b = *g();
}

/* The indirect load should be replaced by a load from a and a
   conversion to int.  */

/* { dg-final { scan-tree-dump "= a;" "forwprop1" } } */
/* { dg-final { scan-tree-dump "= \\\(int\\\) " "forwprop1" } } */
/* { dg-final { scan-tree-dump-not "= \\\*" "forwprop1" } } */
/* { dg-final { cleanup-tree-dump "forwprop1" } } */
