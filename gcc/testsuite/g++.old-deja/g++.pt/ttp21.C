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

class E : C<D,int>
{
	public:
		int h() { return g(); }
};

int main()
{
	E c;
	c.h();
}
