// PR c++/94162
// { dg-do compile { target c++20 } }

#include <compare>

struct S {
  int a;			// { dg-error "three-way comparison of 'S::a' has type 'std::strong_ordering', which does not convert to 'int\\*'" }
  int *operator<=>(const S&) const = default;
};

bool b = S{} < S{};		// { dg-error "use of deleted function 'constexpr int\\* S::operator<=>\\\(const S&\\\) const'" }
