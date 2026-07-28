/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-fre1" } */

/* Variable-addend variant of ssa-fre-113.c: the narrow operation is
   seen first, the widened form of it second.  Uses __PTRDIFF_TYPE__ so
   the test degenerates gracefully on ilp32 targets.  */

int func1(int *a, int j, int i) {
  int k = j + i;
  int x = a[k];
  __PTRDIFF_TYPE__ idx = (__PTRDIFF_TYPE__)j + i;
  return x == a[idx];
}

int func2(int *a, int j, int i) {
  int k = j - i;
  int x = a[k];
  __PTRDIFF_TYPE__ idx = (__PTRDIFF_TYPE__)j - (__PTRDIFF_TYPE__)i;
  return x == a[idx];
}

/* { dg-final { scan-tree-dump-times "return 1;" 2 "fre1" } } */
