// Build don't link:

template<int> class D
{
};

template<template<int> class D,class E> class C
{
	D<int> d;			// ERROR - arg not match
};

int main()
{
	C<D,int> c;
}
