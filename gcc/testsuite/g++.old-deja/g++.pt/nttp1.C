// { dg-do run  }
// Test for nested template template parameter feature

template <template<template <class> class> class TTT> struct C
{
	int f();
};

template <template<template <class> class> class TTT> int C<TTT>::f()
{
	return 0;
}

template <template <class> class TT> struct D
{
};

int main()
{
	C<D> c;
	c.f();
}
