// PR c++/100592
// { dg-do compile { target c++11 } }

template<bool>
struct meta {
  template<class> using if_c = int&;
};

template<bool B>
typename meta<B>::template if_c<void> const f();

using type = decltype(f<true>());
using type = int&;
