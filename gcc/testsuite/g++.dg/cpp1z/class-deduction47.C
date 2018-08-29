// PR c++/84376

template<int> struct A {};

template<typename T> T foo() { return T(); }

template<> A foo<A>();		// { dg-error "A" }
