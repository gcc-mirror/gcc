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
		template<template<class> class F> int f(F<int>);
};

template<template<class> class D,class E> 
template<template<class> class F> int C<D,E>::f(F<int>)
{
	F<E> d2;
	return d2.f();
}

int main()
{
	C<D,int> c;
	D<int> d;
	c.f(d);
}
