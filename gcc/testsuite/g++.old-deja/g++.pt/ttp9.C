// { dg-do run  }
template<class E,class F=int> class D
{
};

template<template<class> class D,class E> class C
{
	D<E>	d;
};

int main()
{
	C<D,int> c;
}
