// Build don't link:

template<class E> class D
{
};

template<template<class> class D,int> class C
{
};

int main()
{
	C<1,D> c;		// ERROR - args not match
}
