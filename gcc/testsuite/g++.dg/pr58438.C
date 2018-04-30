/* { dg-do compile { target i?86-*-* x86_64-*-* } } */
/* { dg-options "-march=amdfam10 -O3 -fprofile-generate -Wno-return-type" } */
enum gimple_code {};
struct A {
  gimple_code code;
};
struct B {
  A gsbase;
};
int **a;
int b, d, e, f, g, h, i, j, k, l, m, n, o;
gimple_code c, p;
class C {
  virtual unsigned m_fn1();
};
B q;
static int fn1() {
  int r;
  if (k)
    i = 0;
  for (; i; j++) {
    b = c <= 0;
    if (b)
      n = *a[0];
    b = p && c;
    if (b)
      r = *a[0];
    b = q.gsbase.code && c;
    if (b)
      o = *a[0];
    m = o;
    if (e || 1 & r || d || l)
      return 0;
  }
}

class D : C {
  unsigned m_fn1() {
    fn1();
    for (; h; g++)
      for (;; f++)
        ;
  }
};
void fn2() { new D; }
