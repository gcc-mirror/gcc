// PR tree-optimization/82929
// { dg-do compile }
// { dg-options "-O2" }

template <int _Nw> struct A {
  long _M_w[_Nw];
  void m_fn1(A p1) {
    for (int a = 0;; a++)
      _M_w[a] &= p1._M_w[a];
  }
  void m_fn2() {
    for (int b = 0; b < _Nw; b++)
      _M_w[b] = ~_M_w[b];
  }
};
template <int _Nb> struct C : A<_Nb / (8 * 8)> {
  void operator&=(C p1) { this->m_fn1(p1); }
  C m_fn3() {
    this->m_fn2();
    return *this;
  }
  C operator~() { return C(*this).m_fn3(); }
};
struct B {
  C<192> Value;
};
void fn1(C<192> &p1) {
  B c;
  p1 &= ~c.Value;
}
