// Test for nested template template parameter feature

template <template<template <class> class> class TTT> struct C
{
	int f() { return 0; }
};

template <template <class> class TT> struct D
{
	int	a;
};

template <template <class> class TT> struct E
{
	int	a;
	int	b;
};

template <template <template <template<class> class> class> class TTT> 
int g(TTT<E> t)
{
	TTT<D> tt;
	return tt.f();
}

int main()
{
	C<E> c;
	g(c);
}
