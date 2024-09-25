// { dg-do compile { target c++20 } }
// { dg-additional-options "-fconcepts" }

template<typename T, T N>
concept C0 = true;

void f(C0<0>);	// { dg-error "declared void|wrong number" }
