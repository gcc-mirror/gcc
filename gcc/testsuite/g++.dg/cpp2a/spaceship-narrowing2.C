// PR c++/104392
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

struct A {
  unsigned int a:5;
};

constexpr std::strong_ordering
operator<=>(const A & left, const A & right)
{
  return left.a <=> right.a;
}
