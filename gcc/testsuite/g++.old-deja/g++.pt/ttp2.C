// { dg-do assemble  }

template<class E> class D
{
};

template<template<class> class D,int> class C
{
};

int main()
{
	C<1,D> c;		// { dg-error "" } args not match
}
