// PR c++/70097
// { dg-do compile { target c++11 } }

struct T {
	void f() & {}
	void g() && {}
};
using F = void() &;
using G = void() &&;

F T::* f = &T::f;
G T::* g = &T::g;

template<typename> struct U;
template<> struct U<void(T::*)() &> {};
template<> struct U<void(T::*)() &&> {};
U<F T::*> u;
U<G T::*> v;
