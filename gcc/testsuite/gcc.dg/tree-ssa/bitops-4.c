/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-optimized -fdump-tree-ccp1" } */
/* PR tree-optimization/111543 */

void f_or(int a, int b, int *por)
{
        int c = ~a;
        *por = (c | b) | a;
}
void f_and(int a, int b, int *pand)
{
        int c = ~a;
        *pand = (c & b) & a;
}
/* { dg-final { scan-tree-dump-times "pand_\[0-9\]+.D. = 0" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "por_\[0-9\]+.D. = -1" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "pand_\[0-9\]+.D. = 0" 1 "ccp1" } } */
/* { dg-final { scan-tree-dump-times "por_\[0-9\]+.D. = -1" 1 "ccp1" } } */
