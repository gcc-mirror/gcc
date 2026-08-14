/* { dg-do compile } */
/* { dg-additional-options "-O3 -fno-tree-pre" } */
/* { dg-additional-options "-mavx2" { target avx2 } } */

int a;
long b, c;
__attribute__((noinline)) int d() {
  a = 0;
  return 0;
}
void e(char f, char g, long h, long i) {
  char j;
k:
  do {
    j = 2 * f;
    if (!g)
      return;
    d();
    if (g << g)
      goto k;
    unsigned long m = b;
    long n = m >> 8;
    c = n + 2088470516281635866 * i + h * h + 7017580219 * i;
  } while (j < f + 2);
}
