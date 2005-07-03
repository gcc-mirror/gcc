/* { dg-do compile } */
/* { dg-options "-fdump-tree-gimple" } */
int g(int x)
{
   return (x + 10) < 0;
}
/* There should be only x < -10 and no x + 10. */
/* { dg-final { scan-tree-dump-times "< -10" 1 "gimple"} } */
/* { dg-final { scan-tree-dump-times "\\+ 10" 0 "gimple"} } */
/* { dg-final { cleanup-tree-dump "gimple" } } */
