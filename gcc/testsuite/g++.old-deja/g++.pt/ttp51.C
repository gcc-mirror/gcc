template<class E, int i, class F, class G=int, int j=i, class H=E> class D
{
};

template<template<class,int,class,class> class D,class E> class C
{
	D<E,2,char,bool>	d;
};

int main()
{
	C<D,int> c;
}
