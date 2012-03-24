// { dg-options -std=c++1y }

auto f() { return f(); }	// { dg-error "auto" }
