// { dg-do compile }
template<template<class> class D,class E> class C
{
	public:
		int g() { return 1; }
};

template<class T> class D
{
	public:
		int f();
};

template<class T> int D<T>::f()
{
	C<D,D> c;	// { dg-error "" }
	return c.g();	// { dg-error "" }
}

int main()
{
	D<char> d;
	d.f();
}
