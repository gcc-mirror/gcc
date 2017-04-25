/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized-raw" } */

int f(int a, int b, int c){
  c |= 1; // c cannot be 0
  a *= c;
  b *= c;
  return a == b;
}

/* { dg-final { scan-tree-dump-not "bit_ior_expr" "optimized" } } */
