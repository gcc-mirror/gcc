template<class T> class D
{
	T	a;
	public:
		int f();
};

template<class T> int D<T>::f()
{
	return sizeof(T);
}

template<template<class> class D,class E> class C
{
		D<E> d;
	public:
		int f();
};

template<template<class> class D,class E> int C<D,E>::f()
{
	D<E> d2;
	return d2.f();
}

int main()
{
	C<D,int> c;
	c.f();
}
