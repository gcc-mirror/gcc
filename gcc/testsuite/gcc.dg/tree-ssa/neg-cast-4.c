/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-phiopt2-details -fdump-tree-optimized" } */
/* PR tree-optimization/107765 */

int b(int input)
{
    if (input == -__INT_MAX__-1) return input;
    unsigned t = input;
    int tt =  -t;
    return tt;
}

/* { dg-final { scan-tree-dump-not "if " "optimized" } } */
/* { dg-final { scan-tree-dump-times "\\(unsigned int\\)" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "\\(int\\)" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "converted to straightline code" 1 "phiopt2" } } */
