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

template<template<class> class D,class E> int f()
{
	D<E> d;
	return d.f();
}

int main()
{
	f<D,int>();
	f<D,char>();
}
