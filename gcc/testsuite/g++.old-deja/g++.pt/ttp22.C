template<class T> class D
{
	public:
		int f();
};

template<class T> int D<T>::f()
{
	return sizeof(T);
}

template<template<class> class DD,class EE> class C : DD<EE>
{
	public:
		int f();
};

template<template<class> class DD,class EE> int C<DD,EE>::f()
{
	return DD<EE>::f();
}

class E : C<D,int>
{
	public:
		int f() { return C<D,int>::f(); }
};

int main()
{
	E c;
	c.f();
}
