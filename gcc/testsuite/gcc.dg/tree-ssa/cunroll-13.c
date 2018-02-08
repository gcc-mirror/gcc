/* { dg-do compile } */
/* { dg-options "-O3 -fdisable-tree-evrp -fdisable-tree-cunrolli -fdisable-tree-vrp1 -fdump-tree-cunroll-blocks-details" } */

#if __SIZEOF_INT__ < 4
__extension__ typedef __INT32_TYPE__ i32;
#else
typedef int i32;
#endif

struct a {int a[8];int b;};
void
t(struct a *a)
{
  for (i32 i=0;i<123456 && a->a[i];i++)
    a->a[i]++;
}
/* This pass relies on the fact that we do not eliminate the redundant test for i early.
   It is necessary to disable all passes that do so.  At the moment it is evrp, vrp1 and cunrolli.  */
/* { dg-final { scan-tree-dump-times "Loop 1 iterates 123454 times" 1 "cunroll" } } */
/* { dg-final { scan-tree-dump-times "Last iteration exit edge was proved true" 1 "cunroll" } } */
/* { dg-final { scan-tree-dump-times "Exit condition of peeled iterations was eliminated" 1 "cunroll" } } */
/* { dg-final { scan-tree-dump-times "loop with 6 iterations completely unrolled" 1 "cunroll" } } */
/* { dg-final { scan-tree-dump-not "Invalid sum" "cunroll" } } */
