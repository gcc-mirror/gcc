// { dg-do compile { target c++2a } }
// { dg-additional-options "-fconcepts-ts" }

template<typename T, T N>
concept bool C0() { return true; }

void f(C0<0>);
