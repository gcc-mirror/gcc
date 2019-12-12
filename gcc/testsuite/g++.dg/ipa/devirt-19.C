/* We should specialize for &b and devirtualize the call.
   Previously we were failing by considering CLOBBER statement to be
   a type change.  */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-ipa-cp -fipa-cp-clone"  } */
/* { dg-additional-options "-Wno-return-type"  } */

struct A {
  void operator==(const A &);
};
class B {
public:
  A m_fn1();
  A m_fn2();
};
template <typename T, typename M> class C {
public:
  T Key;
  const M &m_fn2(const T &);
  virtual void m_fn1() {}
  B _map;
};

C<int, int> b;
template <typename T, typename M> const M &C<T, M>::m_fn2(const T &) {

  A a = _map.m_fn2();
  a == _map.m_fn1();
  m_fn1();
  static M m;
  return m;
}

void fn1() { b.m_fn2(0); }
/* { dg-final { scan-ipa-dump-times "Discovered a virtual call to a known target" 1 "cp"  } } */

