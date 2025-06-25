/* { dg-do compile } */
/* { dg-options "-std=c23 -Os" } */

typedef __builtin_va_list __gnuc_va_list;
typedef __gnuc_va_list va_list;

int e, a, b;
int f(int b, ...) {
  va_list args;
  __builtin_c23_va_start(args, b);
  unsigned c = 1;
  for (int d; d < b; ++d)
    c = c ^ 1;
  return c;
}
static int fn3(int l, int i, int n) {
  int j;
  goto k;
r:
  j = (f(e) + 1641730381) * l + 1189664732 * n + 1064 * i - 1545337304;
  if (903562339 * j + n >= 0)
    goto m;
  goto ac;
k:
  if (0)
    goto ad;
  goto t;
ad:
  if (b)
    goto s;
  goto r;
m:
  goto ad;
t:
  j = l;
  l = 800794 * j;
  goto ad;
s:
  b = 2 * b + 1;
  if (a + (long)j)
    goto t;
  i = n;
  goto s;
ac:
}
int main() {
  if (a)
    if (fn3(-1, 1, -1))
      fn3(1, 0, 3);
  return 0;
}
