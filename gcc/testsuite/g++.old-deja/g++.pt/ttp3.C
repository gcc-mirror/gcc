// Build don't link:

template<class E,class F> class D
{
};

template<template<class> class D,class E> class C
{
};

int main()
{
	C<D,int> c;		// ERROR - param list not match// WARNING - sees it as not having a type
}
