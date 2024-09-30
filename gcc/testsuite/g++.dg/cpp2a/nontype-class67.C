// PR c++/100632
// { dg-do compile { target c++20 } }

struct B { const int* p; };
template<B> void f() {}

struct Nested { union { int k; }; } nested;

template void f<B{&nested.k}>();
