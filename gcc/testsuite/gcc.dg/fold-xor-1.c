/* { dg-do compile } */
/* { dg-options "-fdump-tree-generic" } */

int f (int a, int b) {
  return ~a ^ ~b;
}

unsigned int g (unsigned int a, unsigned int b) {
  return ~a ^ ~b;
}
/* { dg-final { scan-tree-dump-times "a \\^ b" 2 "generic" } } */
/* { dg-final { cleanup-tree-dump "generic" } } */
