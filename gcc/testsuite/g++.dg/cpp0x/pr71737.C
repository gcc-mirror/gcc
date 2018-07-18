// PR c++/78765
// { dg-do compile { target c++11 } }

template <template <typename ...> class TT>
struct quote {
  template <typename ...Ts>
  using apply = TT<Ts...>;  // { dg-error "pack expansion" }
};

template <typename>
using to_int_t = int;

using t = quote<quote<to_int_t>::apply>::apply<int>;
