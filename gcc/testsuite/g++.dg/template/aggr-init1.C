// PR c++/46903

struct A {};
struct B {
	void *(*a)();
};
template <typename T> void *CreateA() {}
B b = {CreateA<A>};
