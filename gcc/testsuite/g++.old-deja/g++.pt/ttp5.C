// { dg-do assemble  }

template<int> class D
{
};

template<template<int> class D,class E> class C
{
	D<int> d;			// { dg-error "" } arg not match
};

int main()
{
	C<D,int> c;
}
