/* { dg-options "-O2 -fdump-tree-optimized" } */

unsigned f1(unsigned i, unsigned j) {
  if (j != i) __builtin_unreachable();
  return __builtin_sub_overflow_p(i, j, (unsigned)0);
}

unsigned f2(unsigned i, unsigned j) {
  if (j > i) __builtin_unreachable();
  return __builtin_sub_overflow_p(i, j, (unsigned)0);
}

unsigned f3(unsigned i, unsigned j) {
  if (j >= i) __builtin_unreachable();
  return __builtin_sub_overflow_p(i, j, (unsigned)0);
}

unsigned f4(unsigned i, unsigned j) {
  if (j <= i) __builtin_unreachable();
  return __builtin_sub_overflow_p(i, j, (unsigned)0);
}

/* { dg-final { scan-tree-dump-times "return 0" 3 optimized } } */
/* { dg-final { scan-tree-dump-times "return 1" 1 optimized } } */
/* { dg-final { scan-tree-dump-not "SUB_OVERFLOW" optimized } } */
/* { dg-final { scan-tree-dump-not "IMAGPART_EXPR" optimized } } */
