/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

_Bool foo(_Bool x) { return (x << 2) != 0; }
_Bool bar(_Bool x) { return (x << 2) == 0; }

/* { dg-final { scan-tree-dump-not " << " "optimized" } } */
