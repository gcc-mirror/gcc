/* PR tree-optimization/109424 */
/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-phiopt1-details" } */

int f2(int x, int y)
{
    return (x > y) ? ~x : ~y;
}
/* { dg-final { scan-tree-dump " = MAX_EXPR" "phiopt1"} } */
/* { dg-final { scan-tree-dump-times "changed to factor operation out from" 1 "phiopt1"} } */
