// { dg-do run  }
#include <vector>

template<template<class> class D,class E> class C
{
		D<E> d;
	public:
		int size() { return d.size(); }
};

template<template<class> class D,class E> int size(D<E> &d1)
{
	d1.size();
	C<D,E> d2;
	d2.size();
	return 0;
}

int main()
{
	std::vector<int> c1;
	std::vector<char> c2;
	size(c1);
	size(c2);
}
