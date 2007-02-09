/* { dg-do compile } */
/* { dg-options "-fno-signed-zeros -ffinite-math-only -fdump-tree-gimple" } */

_Complex double test1 (double x) { return x + 1.i; }
_Complex double test2 (double x) { return 1 + x * 1.i; }
_Complex double test3 (double x, double y) { return x + y * 1.i; }
_Complex double test4 (double x, double y) { return (x + y * 1.i) * 1.i; }
_Complex double test5 (double x, double y) { return (x + y * 1.i) * -1.i; }

/* { dg-final { scan-tree-dump "COMPLEX_EXPR <x, 1.0e\\+0>" "gimple" } } */
/* { dg-final { scan-tree-dump "COMPLEX_EXPR <1.0e\\+0, x>" "gimple" } } */
/* { dg-final { scan-tree-dump "COMPLEX_EXPR <x, y>" "gimple" } } */
/* { dg-final { scan-tree-dump "D.* = -y;\n.*COMPLEX_EXPR <D.*, x>" "gimple" } } */
/* { dg-final { scan-tree-dump "D.* = -x;\n.*COMPLEX_EXPR <y, D.*>" "gimple" } } */
/* { dg-final { cleanup-tree-dump "gimple" } } */
