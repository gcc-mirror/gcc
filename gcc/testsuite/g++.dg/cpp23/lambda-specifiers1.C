// P1102R2 - Down with ()!
// { dg-do compile { target c++11 } }
// { dg-options "" }

void
foo ()
{
  auto a = [] mutable {};	// { dg-warning "parameter declaration before lambda declaration specifiers only optional with" "" { target c++20_down } }
#if __cpp_constexpr >= 201603L
  auto b = [] constexpr {};	// { dg-warning "parameter declaration before lambda declaration specifiers only optional with" "" { target { c++17 && c++20_down } } }
#endif
#if __cpp_consteval >= 201811L
  auto c = [] consteval {};	// { dg-warning "parameter declaration before lambda declaration specifiers only optional with" "" { target c++20_only } }
#endif
  auto d = [] throw() {};	// { dg-warning "parameter declaration before lambda exception specification only optional with" "" { target c++20_down } }
  auto e = [] noexcept {};	// { dg-warning "parameter declaration before lambda exception specification only optional with" "" { target c++20_down } }
  auto f = [] -> int { return 0; };	// { dg-warning "parameter declaration before lambda trailing return type only optional with" "" { target c++20_down } }
}
