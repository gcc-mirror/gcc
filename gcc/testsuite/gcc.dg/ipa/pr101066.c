/* { dg-do run } */
/* { dg-options "-Os -fno-ipa-cp -fno-inline" } */

int a = 1, c, d, e;
int *b = &a;
static int g(int *h) {
  c = *h;
  return d;
}
static void f(int *h) {
  e = *h;
  *b = 0;
  g(h);
}
int main() {
  f(b);
  if (c)
    __builtin_abort();
  return 0;
}
