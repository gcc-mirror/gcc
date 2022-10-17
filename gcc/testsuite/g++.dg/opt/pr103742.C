// PR debug/103742
// { dg-do compile { target c++17 } }
// { dg-options "-O2 -fnon-call-exceptions --param=early-inlining-insns=82 -fcompare-debug" }

template <typename T> T max(T a, T b) { return a >= b ? a : b; }
template <typename T> T abs(T);
template <int T, int U> struct A {
  long a;
  A(A &x) { a = x.a; }
  A(long);
  A foo(A) {
    if (abs(a) && a == a)
      a = a ? U : T;
    else
      a += a;
    return *this;
  }
  bool operator>=(A) { return a; }
};
struct B {};
struct C {
  A<2147483647, 0> c;
};
struct D {
  A<2147483647, 0> d;
  C e[];
};
struct E : D{} * f;
A<2147483647, 0> bar() {
  A<2147483647, 0> g = g.foo(f->d);
  return max(g, (A<2147483647, 0>)1);
}
E *h;
void baz() {
  h->e[0].c = bar();
}
