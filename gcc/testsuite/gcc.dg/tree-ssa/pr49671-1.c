/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" }  */
volatile int t;
static inline int cvmx_atomic_get32(volatile int *ptr)
{
    return *ptr;
}
void f(void)
{
  while (!cvmx_atomic_get32(&t))
    ;
}

/* { dg-final { scan-tree-dump "\{v\}" "optimized" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */


