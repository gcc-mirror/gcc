// { dg-do run  }
template<class E> class DD
{
};

template<int> class D
{
};

template<template<class> class D,class E> class C
{
	D<E>	d;
};

int main()
{
	C<DD,int> c;
}
