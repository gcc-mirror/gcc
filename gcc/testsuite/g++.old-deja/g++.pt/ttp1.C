// Build don't link:

template<class E> class D
{
};

template<template<class> class D,class E> class C
{
};

int main()
{
	C<int,D> c;		// ERROR - args not match
}
