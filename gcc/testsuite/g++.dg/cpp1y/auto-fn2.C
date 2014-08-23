// { dg-do compile { target c++14 } }

auto f() { return f(); }	// { dg-error "auto" }
