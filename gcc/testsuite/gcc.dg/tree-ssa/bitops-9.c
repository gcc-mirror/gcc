/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized-raw" } */
/* PR tree-optimization/115238 */


#define vector8 __attribute__((vector_size(2*sizeof(int))))

void f(int a, vector8 int *b)
{
        a = 1;
        *b = a | ((~a) ^ *b);
}
/* { dg-final { scan-tree-dump-not     "bit_xor_expr, "     "optimized" } } */
/* { dg-final { scan-tree-dump-times   "bit_ior_expr, "  1  "optimized" } } */
/* { dg-final { scan-tree-dump-times   "bit_not_expr, "  1  "optimized" } } */
