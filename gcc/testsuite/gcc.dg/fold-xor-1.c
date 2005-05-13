/* { dg-do compile } */
/* { dg-options "-fdump-tree-gimple" } */

int f (int a, int b) {
  return ~a ^ ~b;
}

unsigned int g (unsigned int a, unsigned int b) {
  return ~a ^ ~b;
}
/* { dg-final { scan-tree-dump-times "a \\^ b" 2 "gimple" } } */
/* { dg-final { cleanup-tree-dump "gimple" } } */
