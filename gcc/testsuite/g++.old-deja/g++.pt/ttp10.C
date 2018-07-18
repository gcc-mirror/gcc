// { dg-do assemble  }

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
		D d;			// { dg-error "" } D is a template
	public:
		int f();
};

template<template<class> class D,class E> int C<D,E>::f()
{
	return d.f();			// { dg-prune-output "was not declared" }
}

int main()
{
	C<D,int> c;
	c.f();
}
