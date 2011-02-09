/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-einline" } */

void foo0();
inline void bar0() { foo0(); }
void foobar() { bar0(); bar0(); bar0(); }

/* { dg-final { scan-tree-dump "Iterations: 1" "einline" } } */
/* { dg-final { cleanup-tree-dump "einline" } } */
