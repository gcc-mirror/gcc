// PR c++/98545
// { dg-do compile { target c++11 } }
// { dg-options "-Wabi=14 -fabi-compat-version=0" }

class a {
public:
  a();
  template <typename b> a(b);
};
template <class = double> using c = a;
class f {
protected:
  template <class d, class e> void operator()(d, double, e);
};
class i : f {
public:
  template <class... g>
  [[gnu::used]] auto h(g...) -> decltype(operator()(g()...)) {} // { dg-warning "mangled name" }
// { dg-final { scan-assembler "_ZN1i1hIJ1adS1_EEEDTcldtdefpTonclspcvT__EEEDpS2_" } }
};
template <class> class C {
public:
  template <class j> C(j);
  i k() const;
  int operator()() {
    int l = 10;
    c<> m, n;
    operator()(m, l, n);
    return 0;
  }
  int operator()(c<> &, c<> const &, c<> const &) const;
  template <class d, class e> void k(d m, double gamma, e o) const {
    k().h(m, gamma, o);
  }
};
template <class r> int C<r>::operator()(c<> &, c<> const &, c<> const &) const {
  [&](c<> m, double gamma, c<> o) { k(m, gamma, o); };
  return 0;
}
c<> p = C<double>(p)();
