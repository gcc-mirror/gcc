/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

int and_test(long a, long b, long c) {
  int cmp1 = a > b;
  int cmp2 = a > c;
  return cmp1 & cmp2;
}

int ior_test (long a, long b, long c) {
  int cmp1 = a < b;
  int cmp2 = a < c;
  return cmp1 | cmp2;
}

/* { dg-final { scan-tree-dump-times "MAX_EXPR" 2 "optimized" } } */
