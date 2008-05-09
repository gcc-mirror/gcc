/* { dg-do compile } */ 
/* { dg-options "-O2 -fdump-tree-alias-vops" } */
struct a
{
  int length;
  int a1[256];
};

void *malloc(__SIZE_TYPE__ size) __attribute__((malloc));

int f(void)
{
   struct a *a = malloc(sizeof(struct a));
   return a->length;
}
/* { dg-final { scan-tree-dump-times "VDEF <HEAP" 1 "alias"} } */
/* { dg-final { cleanup-tree-dump "alias" } } */
