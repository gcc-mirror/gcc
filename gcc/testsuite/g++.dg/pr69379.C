// PR c++/69379
// { dg-do compile }
// { dg-options "-Wformat" }

typedef int T;
class A {
public:
  template <class D> A(const char *, D);
  template <class Fn, class A1, class A2>
  void m_fn1(const char *, Fn, A1 const &, A2);
};
struct Dict {
  void m_fn2();
};
void fn1() {
  A a("", "");
  typedef void *Get;
  typedef void (Dict::*d)(T);
  a.m_fn1("", Get(), d(&Dict::m_fn2), "");
}
