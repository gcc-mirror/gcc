// Copyright (C) 2001 Free Software Foundation
// Contributed by Kriang Lerdsuwanakij <lerdsuwa@users.sourceforge.net>
// { dg-do run }

extern "C" void abort();

struct A
{
	template <class T> class B {};
};

template <template <class> class TT, class T> struct X
{
	TT<int> y;
	T z;
	int f() { return 0; }
};

template <class T> struct X<T::template B, T>
{
	typename T::template B<int> y;
	T z;
	int f() { return 1; }
};

template <class T> struct C
{
	X<T::template B, A> x;
};

int main()
{
	C<A> c;
	X<A::B, A> x1;
	X<A::B, int> x2;
	if (x1.f() != 1)
		abort();
	if (x2.f() != 0)
		abort();
}
