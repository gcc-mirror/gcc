// Origin PR c++/51289
// { dg-do compile { target c++11 } }

template<typename a, template <typename, typename> class b>
struct foo {
  template <typename t>
  using type = b<a, t>;
  template <typename t>
  b<a, t> funca() {}

  template <typename t>
  type<t> funcb() {}
};

// This is an additional test, to emit an error message when using
// unexpanded parameter packs in an alias declaration.
template <class ... T>
struct S {};

template<class ... T>
using A = S<T>; // { dg-error "parameter packs not expanded" }
