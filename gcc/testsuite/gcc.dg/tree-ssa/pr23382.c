/* { dg-do compile } */ 
/* { dg-options "-O2 -fdump-tree-alias-vops" } */
struct a
{
  int length;
  int a1[256];
};

void *malloc(__SIZE_TYPE__ size) __attribute__((malloc));

void f(void)
{
   struct a *a = malloc(sizeof(struct a));
}
/* { dg-final { scan-tree-dump-times "V_MAY_DEF <HEAP" 1 "alias1"} } */
/* { dg-final { scan-tree-dump-times "V_MAY_DEF <HEAP" 1 "alias2"} } */
/* { dg-final { scan-tree-dump-times "V_MAY_DEF <HEAP" 1 "alias3"} } */
/* { dg-final { scan-tree-dump-times "V_MAY_DEF <HEAP" 1 "alias4"} } */
/* { dg-final { scan-tree-dump-times "V_MAY_DEF <HEAP" 1 "alias5"} } */
/* { dg-final { cleanup-tree-dump "alias1" } } */
/* { dg-final { cleanup-tree-dump "alias2" } } */
/* { dg-final { cleanup-tree-dump "alias3" } } */
/* { dg-final { cleanup-tree-dump "alias4" } } */
/* { dg-final { cleanup-tree-dump "alias5" } } */
/* { dg-final { cleanup-tree-dump "alias6" } } */
