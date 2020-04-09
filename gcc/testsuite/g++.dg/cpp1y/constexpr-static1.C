// PR c++/94523
// { dg-do compile { target c++14 } }

template <bool, typename a> using b = a;
struct d {
  char ao;
  template <typename ap> constexpr d(ap) : ao{} {}
};
template <int... au> struct e { static constexpr auto aw = d(au...); };
template <int c> b<c, d> ax(e<1>::aw);
