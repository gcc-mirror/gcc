/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-optimized" } */

int main()
{
  int *p = __builtin_malloc (4);
  *p = 4;
  return 0;
}

/* { dg-final { scan-tree-dump-not "malloc" "optimized" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
