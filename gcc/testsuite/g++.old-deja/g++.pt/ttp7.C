// { dg-do assemble  }

template<class E> class D
{
};

template<template<class> class D,class E> class C	// { dg-message "" } ref below
{
	D<int,int> d;				// { dg-error "" } arg not match
};

int main()
{
	C<D,int> c;
}
