// PR c++/100893

template<class T, typename T::type F> void g() { }

struct A { typedef void (*const type)(); };
void f();
template void g<A, &f>();

struct B { typedef void (B::*const type)(); void f(); };
template void g<B, &B::f>();
