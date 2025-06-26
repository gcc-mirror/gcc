// C++26 P2830R10 - Constexpr Type Ordering
// { dg-do compile { target c++26 } }

namespace std {
  struct strong_ordering {
  };
}
constexpr auto a = __builtin_type_order (int, long);	// { dg-error "'(equal|greater|less)' is not a member of 'std::strong_ordering'" }
