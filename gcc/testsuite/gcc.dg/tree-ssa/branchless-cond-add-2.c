/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* PR tree-optimization/97711 */

int f (int x) { return x & 1 ? x - 1 : x; }

/* { dg-final { scan-tree-dump-times " & -2" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-not "if " "optimized" } } */
