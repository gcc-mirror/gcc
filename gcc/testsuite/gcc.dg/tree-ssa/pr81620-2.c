/* { dg-do run } */
/* { dg-options "-O3 -fno-tree-loop-vectorize -fdump-tree-pcom-details" } */

int a[200];
char b;
void abort (void);

int main() {
  int i;
  b = 100;
  for (; b; b--) {
    a[b] = 2;
    a[b + 2] = 1;
  }

  if (a[0] != 0 || a[1] != 2 || a[2] != 2)
    abort ();
  for (i = 3; i < 103; i++)
    if (a[i] != 1)
    abort ();

  return 0;
}

/* { dg-final { scan-tree-dump-times "Store-stores chain" 1 "pcom" } } */
