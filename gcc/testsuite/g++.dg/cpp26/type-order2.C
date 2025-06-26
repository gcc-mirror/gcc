// C++26 P2830R10 - Constexpr Type Ordering
// { dg-do compile { target c++26 } }

constexpr auto a = __builtin_type_order (int, long);	// { dg-error "'strong_ordering' is not a member of 'std'" }
