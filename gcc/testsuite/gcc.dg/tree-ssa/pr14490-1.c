/* { dg-do compile } */
/* { dg-options "-fstrict-overflow -fdump-tree-gimple" } */
int g(int x)
{
   return (x - 10) < 0;
}
/* There should be only x <= 9 and no x - 10. */
/* { dg-final { scan-tree-dump-times "<= 9" 1 "gimple"} } */
/* { dg-final { scan-tree-dump-times "\\+ -10" 0 "gimple"} } */
/* { dg-final { cleanup-tree-dump "gimple" } } */

