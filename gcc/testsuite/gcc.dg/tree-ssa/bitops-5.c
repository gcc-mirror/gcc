/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized-raw" } */
/* PR tree-optimization/111679 */

int f1(int a, int b)
{
        return (~a) | (a ^ b); // ~(a & b) or (~a) | (~b)
}

_Bool fb(_Bool c, _Bool d)
{
        return (!c) | (c ^ d); // ~(c & d) or (~c) | (~d)
}

_Bool fb1(int x, int y)
{
        _Bool a = x == 10,  b = y > 100;
        return (!a) | (a ^ b); // ~(a & b) or (~a) | (~b)
        // or (x != 10) | (y <= 100)
}

/* { dg-final { scan-tree-dump-not   "bit_xor_expr, "   "optimized" } } */
/* { dg-final { scan-tree-dump-times "bit_not_expr, " 2 "optimized" } } */
/* { dg-final { scan-tree-dump-times "bit_and_expr, " 2 "optimized" } } */
/* { dg-final { scan-tree-dump-times "bit_ior_expr, " 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "ne_expr, _\[0-9\]+, x_\[0-9\]+"      1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "le_expr, _\[0-9\]+, y_\[0-9\]+"      1 "optimized" } } */
