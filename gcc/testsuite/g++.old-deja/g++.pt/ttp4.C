// { dg-do assemble  }

template<class E> class D
{
};

template<template<class> class D,class E> class C
{
	D<1> d;			// { dg-error "" } arg not match
};

int main()
{
	C<D,int> c;
}
