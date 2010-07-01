/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-fre-details" } */

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
   conversion to int.  FRE should then be able to replace
   the rhs of the store to b by 1.  */

/* { dg-final { scan-tree-dump "Replaced\[^\\n\]*with 1" "fre" } } */
/* { dg-final { cleanup-tree-dump "fre" } } */
