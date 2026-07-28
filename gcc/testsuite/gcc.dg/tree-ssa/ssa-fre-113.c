/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-fre1" } */

int func1(int *a, int j) {
  int k = j - 1;
  return a[j - 1] == a[k];
}

int func2(int *a, int j) {
  int k = j - 1;
  return a[k] == a[j-1];
}

/* { dg-final { scan-tree-dump-times "return 1;" 2 "fre1" } } */
