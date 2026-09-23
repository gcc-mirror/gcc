/* { dg-do run } */

short a, *b;
signed char c, i = 11, j, k, l, m;
signed char tt;
__attribute__((noinline)) int t(signed char r) {
  tt = 1;
  return 0;
}
int main() {
  short n = 20158;
  while (1) {
    a = ~n;
    c = n;
    if (a < -32255)
      break;
    j = n;
    k = j % i;
    m = l = 5;
    m = m * k % i * k;
    l = l + 8 * k + m;
    l = l % i;
    t(l);
    n = 6303 + n;
  }
  if (l != 5)
    __builtin_abort ();
  return 0;
}
