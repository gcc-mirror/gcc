/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized -fdump-tree-forwprop1-details" } */

/* PR tree-optimization/118946 */

void f(char *a)
{
  char t[1024] = {};
  __builtin_memcpy(a, t, 10);
}

/* We should be able to optimize the memcpy into a memset here. */
/* { dg-final { scan-tree-dump-times "after previous" 1 "forwprop1"} } */
/* { dg-final { scan-tree-dump-times "memset " 1 "optimized" } } */
/* { dg-final { scan-tree-dump-not "memcpy " "optimized" } } */
