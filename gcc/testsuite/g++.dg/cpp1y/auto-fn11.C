// { dg-do compile { target c++1y } }

auto f() { return; } 		// OK, return type is void
auto* g() { return; }		// { dg-error "no value" }
auto* h() { }			// { dg-error "no return statements" }
