/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized -fdump-tree-cddce1" } */
/* PR tree-optimization/107765 */

int a(int input)
{
    if (input == -__INT_MAX__-1) return 1;
    unsigned t = input;
    int tt =  -t;
    return tt == -input;
}

int b(int input)
{
    if (input == -__INT_MAX__-1) __builtin_trap();
    unsigned t = input;
    int tt =  -t;
    return tt;
}

/* { dg-final { scan-tree-dump "return 1" "optimized" } } */
/* { dg-final { scan-tree-dump-not "\\(unsigned int\\)" "cddce1" } } */
/* { dg-final { scan-tree-dump-not "\\(int\\)" "cddce1" } } */
