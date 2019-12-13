// { dg-do compile { target c++11 } }

auto l = []() [[noreturn]] {};	// { dg-warning "does not apply to types" }
