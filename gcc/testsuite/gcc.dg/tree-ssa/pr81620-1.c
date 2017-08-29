/* { dg-do run } */
/* { dg-options "-O3 -fno-tree-loop-vectorize -fdump-tree-pcom-details" } */

int a[7];
char b;
void abort (void);

int main() {
  b = 4;
  for (; b; b--) {
    a[b] = b;
    a[b + 2] = 1;
  }
  if (a[0] != 0 || a[1] != 1 || a[2] != 2
      || a[3] != 1 || a[4] != 1 || a[5] != 1 || a[6] != 1)
    abort ();
  return 0;
}

/* { dg-final { scan-tree-dump-times "Store-stores chain" 1 "pcom" } } */
