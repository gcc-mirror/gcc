// PR c++/90842
// { dg-do compile { target c++14 } }

auto a = [](auto x) struct C { void foo (); } {};	// { dg-error "expected" }
							// { dg-error "type-specifier invalid in lambda" "" { target *-*-* } .-1 }
auto b = [](auto x) mutable typedef {};			// { dg-error "'typedef' invalid in lambda" }
#if __cpp_concepts >= 201907L
auto c = [](auto x) constexpr concept {};		// { dg-error "'concept' invalid in lambda" "" { target c++2a } }
#endif
auto d = [](auto x) mutable friend {};			// { dg-error "'friend' invalid in lambda" }
