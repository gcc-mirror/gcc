// PR c++/37965
// Negative test for auto
// { dg-options "-std=c++0x" }

auto i = 6;
auto j;			// { dg-error "has no initializer" }

template<int> struct A
{
  static auto k = 7;	// { dg-error "non-const" }
  static auto l;	// { dg-error "has no initializer" }
  auto m;		// { dg-error "non-static data member declared" }
};
