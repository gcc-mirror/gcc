/* { dg-do compile } */ 
/* { dg-options "-O2 -fno-tree-dominator-opts -fdump-tree-fre1-stats" } */
int
foo (unsigned long a)
{
  int b = __builtin_clzl (a);
  int c = __builtin_clzl (a);
  if (b == c)
    return 1;
  return 0;
}
/* { dg-final { scan-tree-dump-times "return 0;" 0 "fre1"} } */
/* { dg-final { cleanup-tree-dump "fre1" } } */
