template<class T> class D
{
	public:
		int f() const;
};

template<class T> int D<T>::f() const
{
	return sizeof(T);
}

template<template<class> class D,class E> class C
{
		D<E> d;
	public:
		int f() const { return d.f(); }
};

template<template<class> class D,class E> int f(const D<E> &d1)
{
	d1.f();
	C<D,E> d2;
	d2.f();
	return 0;
}

int main()
{
	D<const int> c1;
	D<char> c2;
	const D<char> c3(c2);
	f(c1);
	f(c2);
	f(c3);
}
