/* { dg-do compile } */
/* { dg-options "-O2 -std=c++11 -w" } */

template <typename _T1, typename _T2> struct A {
  _T1 first;
  _T2 second;
  template <typename _U1, typename _U2>
  A(_U1 &&p1, _U2 p2) : first(p1), second(p2) {}
};
class B {
public:
  B(char *);
};
struct C {
  virtual ~C();
};
struct D {
  virtual void registerReporter(B const &, int *) = 0;
};
template <typename, typename, typename, typename, typename = int> class F {
public:
  void _M_get_insert_unique_pos();
  template <typename... _Args> A<int, bool> _M_emplace_unique(_Args &&...);
};
template <typename _Key, typename _Val, typename _KeyOfValue, typename _Compare,
          typename _Alloc>
template <typename... _Args>
A<int, bool> F<_Key, _Val, _KeyOfValue, _Compare, _Alloc>::_M_emplace_unique(
    _Args &&...) try {
  _M_get_insert_unique_pos();
} catch (...) {
}
class G {
  F<int, int, int, int> _M_t;

public:
  void insert(A<const B &, int *&> p1) { _M_t._M_emplace_unique(p1); }
};
class H {
public:
  void registerReporter(B const &p1, int *p2) {
    A<const B &, int *&> a(p1, p2);
    m_factories.insert(a);
  }
  G m_factories;
};
namespace {
class J : C, D {
  void registerReporter(B const &p1, int *p2) {
    m_reporterRegistry.registerReporter(p1, p2);
  }
  H m_reporterRegistry;
};
J fn1();
} // namespace
void fn2() { fn1(); }
D &fn3();
class I {
public:
  I(B p1) { fn3().registerReporter(p1, new int); }
};
namespace {
I b("");
}
