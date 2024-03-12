// CWG 2543
// { dg-do compile { target c++20 } }

float f;
constinit int * pi = (int*) &f;	// { dg-error "constant" } reinterpret_cast, not constant-initialized
