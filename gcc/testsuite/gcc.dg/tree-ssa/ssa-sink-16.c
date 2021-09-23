/* { dg-do compile } */
/* Note PRE and DOM jump threading rotate the loop and blocks the sinking opportunity.  */
/* { dg-options "-O2 -fno-tree-pre -fno-tree-dominator-opts -fdump-tree-sink -fdump-tree-optimized" } */

int f(int n)
{
  int i,j=0;
  for (i = 0; i < 31; i++)
    j = __builtin_ffs(i);
  return j;
}

/* { dg-final { scan-tree-dump "Sinking j_. = __builtin_ffs" "sink1" } } */
/* { dg-final { scan-tree-dump "return 2;" "optimized" } } */
