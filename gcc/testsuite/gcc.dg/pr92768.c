/* PR tree-optimization/92768 */
/* { dg-options "-O2 -fno-signed-zeros -fdump-tree-optimized -w -Wno-psabi" } */

typedef float v4sf __attribute__((vector_size(16)));
v4sf f () { return (v4sf) { 0.0, -0.0, 0.0, -0.0 }; }

/* { dg-final { scan-tree-dump {{ 0\.0, -0\.0, 0\.0, -0\.0 }} "optimized" } } */
