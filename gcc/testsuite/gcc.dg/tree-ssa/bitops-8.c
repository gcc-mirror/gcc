/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized-raw" } */
/* PR tree-optimization/115224 */

int f1(int a, int b)
{
        a = a ^ 1;
        int c = ~a;
        return c | (a ^ b);
        // ~((a ^ 1) & b) or (a ^ -2) | ~b
}
/* { dg-final { scan-tree-dump-times   "bit_xor_expr, "  1  "optimized" } } */
/* { dg-final { scan-tree-dump-times   "bit_ior_expr, "  1  "optimized" } } */
/* { dg-final { scan-tree-dump-times   "bit_not_expr, "  1  "optimized" } } */

