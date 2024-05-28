/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-cddce1-raw" } */
/* PR tree-optimization/115238 */


#define vector8 __attribute__((vector_size(2*sizeof(int))))

void f(int a, vector8 int *b)
{
        a = 1;
        *b = a | ((~a) ^ *b);
}
/* Scan early on in the phases before the vector has possibily been split
   but late enough after forwprop or other match-simplify has happened though. */
/* { dg-final { scan-tree-dump-not     "bit_xor_expr, "     "cddce1" } } */
/* { dg-final { scan-tree-dump-times   "bit_ior_expr, "  1  "cddce1" } } */
/* { dg-final { scan-tree-dump-times   "bit_not_expr, "  1  "cddce1" } } */
