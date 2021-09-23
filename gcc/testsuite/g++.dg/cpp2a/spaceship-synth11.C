// PR c++/99429
// { dg-do compile { target c++20 } }

namespace std {
struct strong_ordering {
  int _v;
  constexpr strong_ordering (int v) :_v(v) {}
  constexpr operator int (void) const { return _v; }
  static const strong_ordering less;
  static const strong_ordering equal;
  static const strong_ordering greater;
};
constexpr strong_ordering strong_ordering::less = -1;
constexpr strong_ordering strong_ordering::equal = 0;
constexpr strong_ordering strong_ordering::greater = 1;
}

template <unsigned long N>
struct duration {
  static constexpr const long period = N;
  constexpr duration (void) = default;
  constexpr duration (const duration& d) = default;
  constexpr bool operator== (const duration& d) const = default;
  constexpr bool operator<=> (const duration& d) const = default;
  long _d;
};

using nanoseconds = duration<1>;
using microseconds = duration<nanoseconds::period * 1000>;
