// { dg-do assemble  }

template<class E> class D
{
};

template<template<class> class D,class E> class C
{
};

int main()
{
	C<int,D> c;		// { dg-error "" } args not match
}
