template<int T, class U = int> class D
{
	public:
		int f();
};

template<int T, class U> int D<T,U>::f()
{
	return T+sizeof(U);
}

template<template<int> class D,class E> class C
{
		D<1> d;
	public:
		int f() { return d.f(); }
};

template<template<int> class D, int T> int f(D<T> &d1)
{
	d1.f();
	return T;
}

int main()
{
	D<1> c1;
	f(c1);
}
