// Build don't link:

template<class E,class F> class D
{
};

template<template<class> class D,class E> class C
{				// ERROR - ref below
};

int main()
{
	C<D,int> c;		// ERROR - param list not match
}
