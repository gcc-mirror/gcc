// PR c++/78774 - [6/7 Regression] ICE in constexpr string literals and
// templates
// { dg-do compile { target c++14 } }

template <int> struct ops {
  template <int> struct A;
  template <int *Ptr> using explode = typename A<*Ptr>::join;
};
template <typename Ts> typename ops<'\0'>::explode<Ts::join>::type a;
