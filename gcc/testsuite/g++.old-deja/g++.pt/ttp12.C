template<class T> class D
{
	public:
		int f();
};

template<class T> int D<T>::f()
{
	return sizeof(T);
}

template<template<class> class E,class D> class C
{
		E<D> d;
	public:
		int f();
};

template<template<class> class E,class D> int C<E,D>::f()
{
	return d.f();
}

int main()
{
	C<D,int> c;
	c.f();
}
