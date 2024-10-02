// { dg-options "-std=c++98" }

#include <cstddef>

int
operator ""_mm(long double m)	// { dg-warning "user-defined literals only available with" }
{ return int(1000.0L * m); }

int in = 0.0254_mm;	// { dg-error "invalid suffix" }

int
operator ""_Q(const char *, std::size_t)	// { dg-warning "user-defined literals only available with" }
{ return 42; }

int x = "Hello"_Q;	// { dg-error "invalid conversion from" "invalid" }

// { dg-error "expected" "expected" { target *-*-* } 15 }
