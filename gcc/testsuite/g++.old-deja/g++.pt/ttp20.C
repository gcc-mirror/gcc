template<class T> class D
{
	public:
		int f();
};

template<class T> int D<T>::f()
{
	return sizeof(T);
}

template<template<class> class D,class E> class C : D<E>
{
	public:
		int g();
};

template<template<class> class D,class E> int C<D,E>::g()
{
	return f();
}

int main()
{
	C<D,int> c;
	c.g();
}
