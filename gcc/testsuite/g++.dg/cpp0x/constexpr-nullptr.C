// { dg-options -std=c++0x }

constexpr int zero() { return 0; }

void* ptr1 = zero();		// { dg-error "int" }
constexpr void* ptr2 = zero();	// { dg-error "int" }
