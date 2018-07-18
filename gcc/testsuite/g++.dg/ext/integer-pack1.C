// { dg-do compile { target c++11 } }

template <int... I> struct A { };

template <int N>
using TS = A<__integer_pack(N)...>;

TS<4> t = 1;			// { dg-error "A<0, 1, 2, 3>" }

template <int N>
using TS2 = A<__integer_pack(N)...>; // { dg-error "argument" }

TS2<-1> t2;

template <int N>
using TS2 = A<__integer_pack(N)>; // { dg-error "not expanded" }

template <int N>
using TS3 = A<__integer_pack>; // { dg-error "" }

int i = __integer_pack(2);	// { dg-error "__integer_pack" }
int j = __integer_pack(2)...;	// { dg-error "__integer_pack" }
