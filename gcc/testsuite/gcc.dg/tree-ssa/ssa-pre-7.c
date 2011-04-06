/* { dg-do compile } */ 
/* { dg-options "-O2 -fno-tree-dominator-opts -fdump-tree-fre1-stats" } */
int
foo (int *array)
{
    if (array[1] != 0)
          return array[1];
      return 0;
}
/* We should eliminate one load.  */
/* { dg-final { scan-tree-dump-times "Eliminated: 1" 1 "fre1"} } */
/* { dg-final { cleanup-tree-dump "fre1" } } */
