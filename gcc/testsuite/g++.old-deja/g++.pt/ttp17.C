template<class T> class D
{
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
		int g() { return 0; }
};

template<template<class> class D,class E> int C<D,E>::f()
{
	C<D,E> d2;
	return d2.g();
}

int main()
{
	C<D,int> c;
	c.f();
}
