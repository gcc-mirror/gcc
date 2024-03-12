/* PR tree-optimization/109722 */
/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-gimple -fdump-tree-optimized" } */

int g(signed char x){
    x = x < 0 ? -x : x;
    return x == 0;
}

/* This should work even if int is 16bits. */
/* { dg-final { scan-tree-dump "ABSU_EXPR" "gimple"} } */
/* { dg-final { scan-tree-dump-not "ABSU_EXPR" "optimized"} } */
