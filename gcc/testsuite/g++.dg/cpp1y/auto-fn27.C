// PR c++/66919
// { dg-do compile { target c++14 } }

template <int>
struct A {
  template <typename F, typename R, typename T>
    static auto run (F fn, R, T) { auto r =  fn (); } // { dg-error "" }
};
template <typename F, typename T>
auto foo (F fn, T)
{
  A <0>::run (fn, 0, 0);
}
struct B;
struct C {
  typedef B D;
};
struct E {
  virtual void bar (const int &);
};
template <typename C>
struct F : E {
  typedef typename C::D::G G;
  void bar (const G &);
  typename C::D::H I;
};
struct J { struct K {}; };
template <typename T>
void
F<T>::bar (const G &)
{
  auto s = I;
  typedef decltype (s) L;
  auto u =[&](L) { auto t = foo (J::K (), 0); }; // { dg-error "25:declared void" }
}
struct B {
  typedef int G;
  typedef int H;
};
struct M : F <C> {
  M () {}
};
