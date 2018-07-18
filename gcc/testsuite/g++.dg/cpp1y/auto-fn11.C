// { dg-do compile { target c++14 } }
// { dg-additional-options "-Wno-return-type" }

auto f() { return; } 		// OK, return type is void
auto* g() { return; }		// { dg-error "no value" }
auto* h() { }			// { dg-error "no return statements" }
