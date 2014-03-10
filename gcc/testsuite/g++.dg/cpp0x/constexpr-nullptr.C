// { dg-do compile { target c++11 } }

constexpr int zero() { return 0; }

void* ptr1 = zero();		// { dg-error "int" }
constexpr void* ptr2 = zero();	// { dg-error "int" }
