template<int> class F
{
};

template<template<int> class D,class E> class C
{
	D<1> d;
};

int main()
{
	C<F,int> c;
}
