/* { dg-do run } */
/* { dg-options "-Os -fno-tree-sra" } */

int printf(const char *, ...);
int a, b = 1, c;
int main() {
  int d[2], *e = &d[1];
  while (a) {
    int *f = &b;
    d[1] = 0;
    *f = 0;
  }
  if (c)
    printf("%d\n", *e);
  if (b != 1)
    __builtin_abort();
  return 0;
}
