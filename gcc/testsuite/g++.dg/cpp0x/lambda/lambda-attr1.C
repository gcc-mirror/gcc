// { dg-do compile { target c++11 } }

auto l = []() [[noreturn]] {};	// { dg-warning "ignored" }
