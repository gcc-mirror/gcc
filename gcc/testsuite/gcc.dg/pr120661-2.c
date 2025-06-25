/* { dg-do compile } */
/* { dg-options "-std=c23 -O2" } */

typedef __builtin_va_list __gnuc_va_list;
typedef __gnuc_va_list va_list;

int a, c, d;
int e(int b, ...) {
    va_list args;
    __builtin_c23_va_start(args, b);

    int r = 0;
    for (int i = 0; i < b; i++) {
        int v = __builtin_va_arg(args, int);
        r += v;
    }
    __builtin_va_end (args);
    return r;
}
int f() { e(0); }
int main() {
  int h = 0, g = 0;
  goto l;
i:
  if (f() * h)
    goto k;
j:
  h = h - 2;
k:
  d = 1200000000 * h + 10;
  g = (long)g + -1000000000 * d + 1;
  if (a * h >= 0) {
    if (g + (c - (long)1) >= 0)
      goto i;
    return 0;
  }
l:
  goto j;
}
