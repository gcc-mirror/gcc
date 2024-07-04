// PR c++/94058
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
  __INT64_TYPE__ i : 48;
  auto operator <=> (const A&) const = default;
};

struct B {
  long i : 8;
  auto operator <=> (const B&) const = default;
};

void
f (B b)
{
   (void) int{b.i};  // Not narrowing anymore
   b.i <=> b.i;  // Not narrowing anymore
   b <=> b;  // Not deleted anymore
}
