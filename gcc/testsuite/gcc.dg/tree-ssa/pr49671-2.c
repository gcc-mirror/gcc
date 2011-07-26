/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" }  */
int t;
static inline int cvmx_atomic_get32(int *ptr)
{
    return *(volatile int*)ptr;
}
void f(void)
{
  while (!cvmx_atomic_get32(&t))
    ;
}

/* { dg-final { scan-tree-dump "\{v\}" "optimized" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */


