// { dg-do compile { target c++1y } }

auto f() { return f(); }	// { dg-error "auto" }
