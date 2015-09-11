// PR c++/66536
// { dg-do compile { target c++14 } }

template <typename> struct make_impl;
struct Tuple;
template <> struct make_impl<Tuple> {};
struct A {
  template <typename X> auto operator()(X) { return make_impl<Tuple>(); }
};
template <typename> A make;
template <typename _Tp, int> struct array { _Tp _M_elems; };
struct Tracked {
  Tracked(int);
};
struct B {
  Tracked tracker{0};
};
template <int> using ct_eq = B;
auto eq_arrays = make<Tuple>(array<ct_eq<0>, 0>{});
