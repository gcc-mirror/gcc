// { dg-do compile }
// { dg-options "-O2 -Winline" }
// Origin: <markus at oberhumer dot com>
// PR 17115: We should not emit -Winline warning for functions marked with
//  noinline

struct Foo {
  __attribute__((noinline)) int a(int r) { return r & 1; }
  virtual __attribute__((noinline)) int b(int r) { return r & 1; }
  static  __attribute__((noinline)) int c(int r) { return r & 1; }
};

int bar(int r) {
  Foo f;
  int k = 1; k &= f.a(r); k &= f.b(r); k &= f.a(r);
  return k;
}
