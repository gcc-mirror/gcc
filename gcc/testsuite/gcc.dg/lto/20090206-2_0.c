/* { dg-lto-do link } */
/* { dg-skip-if "" { ! { i?86-*-linux* i?86-*-gnu* x86_64-*-linux* } } { "*" } { "" } } */
/* { dg-require-effective-target fpic } */
/* { dg-lto-options {{-flto -flto-partition=1to1 -fPIC}} } */
/* { dg-suppress-ld-options {-fPIC} } */

void func(int n) {
  static int __thread v = 0;
  int i;
  for (i = 0; i < n; ++i) {
    volatile int *p = &v;
    volatile int x __attribute__ ((unused)) = *p;
  }
}

int main(int argc, char **argv) {
  func(argc);
  return 0;
}
