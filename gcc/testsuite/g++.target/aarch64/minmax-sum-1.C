/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

/* Integer vector forms use the same simplification as scalar integers.  */

typedef int v4si __attribute__((vector_size (16)));
typedef unsigned int v4ui __attribute__((vector_size (16)));

v4si f1 (v4si a, v4si b) { v4si m = a < b ? a : b; return (a + b) - m; }
v4si f2 (v4si a, v4si b) { v4si m = a < b ? b : a; return (a + b) - m; }
v4ui f3 (v4ui a, v4ui b) { v4ui m = a < b ? a : b; return (a + b) - m; }
v4ui f4 (v4ui a, v4ui b) { v4ui m = a < b ? b : a; return (a + b) - m; }

/* { dg-final { scan-tree-dump-times "MAX_EXPR" 2 "optimized" } } */
/* { dg-final { scan-tree-dump-times "MIN_EXPR" 2 "optimized" } } */
/* { dg-final { scan-tree-dump-not " \\+ " "optimized" } } */
/* { dg-final { scan-tree-dump-not " - " "optimized" } } */
