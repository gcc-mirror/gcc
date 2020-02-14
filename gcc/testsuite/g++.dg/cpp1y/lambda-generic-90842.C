// PR c++/90842
// { dg-do compile { target c++14 } }

auto a = [](auto x) struct C { void foo (); } {};	// { dg-error "expected" }
							// { dg-error "type-specifier invalid in lambda" "" { xfail *-*-* } .-1 }
auto b = [](auto x) mutable typedef {};			// { dg-error "'typedef' invalid in lambda" }
auto d = [](auto x) mutable friend {};			// { dg-error "'friend' invalid in lambda" }
