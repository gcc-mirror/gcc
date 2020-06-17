// PR c++/93650
// { dg-do compile { target c++20 } }

namespace std {
  using type = enum _Ord { less };
  class strong_ordering {
    type _M_value;
    constexpr strong_ordering(_Ord) : _M_value() {}
    static const strong_ordering less;
    static strong_ordering equal;
    static strong_ordering greater;
  } constexpr strong_ordering::less(_Ord::less);
  auto v = 1 <=> 2;
}
