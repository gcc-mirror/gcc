/* { dg-do run } */

int printf(const char *, ...);
int a[10], b, c, d[0], h, i, j, k, l;
signed char e = -1, g;
volatile int f;
static void n() {
  while (e >= 0)
    while (1)
      ;
  for (b = 2; b >= 0; b--) {
    for (k = 0; k < 4; k++) {
      if (e || i)
        continue;
      for (h = 0; h < 2; h++)
        f;
    }
    for (l = 2; l >= 0; l--)
      g = 0;
    for (; g < 1; g++)
      if (c)
        d[l] = 1;
    a[9] = 0;
    a[b] = 1;
    while (j)
      printf("\n");
  }
}
int main() {
  n();
  if (a[1] != 1)
    __builtin_abort();
  return 0;
}
