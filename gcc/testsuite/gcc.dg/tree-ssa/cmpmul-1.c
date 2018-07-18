/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized-raw" } */

int f(int a, int b, int c){
  if (c == 0) __builtin_unreachable();
  a *= c;
  b *= c;
  return a == b;
}

/* { dg-final { scan-tree-dump-not "bit_ior_expr" "optimized" } } */
