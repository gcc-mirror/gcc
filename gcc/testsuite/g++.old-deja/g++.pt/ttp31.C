template<class T> class D
{
	public:
		int f();
};

template<class T> int D<T>::f()
{
	return sizeof(T);
}

extern "C" void abort();

template<template<class> class D,class E> class C
{
		D<E> d;
	public:
		int f() { abort(); return 0; }
};

template<template <class> class F> class C<F,int>
{
		F<int> d;
	public:
		int f() { return d.f(); }
};

int main()
{
	C<D,int> c;
	c.f();
}
