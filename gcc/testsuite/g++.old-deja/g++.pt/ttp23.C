// { dg-do run  }
template<class T> class D
{
	public:
		int f();
};

template<class T> int D<T>::f()
{
	return sizeof(T);
}

template<template<class> class E,class D> class C : E<D>
{
	public:
		int f();
};

template<template<class> class E,class D> int C<E,D>::f()
{
	return E<D>::f();
}

class E : C<D,int>
{
	public:
		int f() { return C<D,int>::f(); }
};

int main()
{
	E c;
	c.f();
}
