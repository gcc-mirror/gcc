template<class T, class U = int> class D
{
	public:
		int f();
};

template<class T, class U> int D<T,U>::f()
{
	return sizeof(T)+sizeof(U);
}

template<template<class> class D,class E> class C
{
		D<E> d;
	public:
		int f() { return d.f(); }
};

template<template<class> class D> int f(D<int> &d1)
{
	d1.f();
	return 0;
}

int main()
{
	D<int> c1;
	f(c1);
}
