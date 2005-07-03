/* { dg-do compile } */
/* { dg-options "-fdump-tree-gimple -fwrapv" } */
int g(int x)
{
   return (x + 10) < 0;
}
/* There should be no x < -10 and one x + 10. */
/* { dg-final { scan-tree-dump-times "< -10" 0 "gimple"} } */
/* { dg-final { scan-tree-dump-times "\\+ 10" 1 "gimple"} } */
/* { dg-final { cleanup-tree-dump "gimple" } } */
