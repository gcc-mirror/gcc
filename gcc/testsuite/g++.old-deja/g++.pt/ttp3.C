// { dg-do assemble  }

template<class E,class F> class D
{
};

template<template<class> class D,class E> class C
{
};

int main()
{
	C<D,int> c;		// { dg-error "" } param list not match/sees it as not having a type
}
