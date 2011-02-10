// PR c++/46903
// This isn't C++0x code, but it was breaking in C++0x mode.
// { dg-options -std=c++0x }

struct A {};
struct B {
	void *(*a)();
};
template <typename T> void *CreateA() {}
B b = {CreateA<A>};
