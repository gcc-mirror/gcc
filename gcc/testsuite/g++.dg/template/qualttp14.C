// Copyright (C) 2001 Free Software Foundation
// Contributed by Kriang Lerdsuwanakij <lerdsuwa@users.sourceforge.net>
// { dg-do link }

struct A
{
	template <class T> class B {};
};

template <template <class> class TT, class T> struct X
{
	TT<int> y;
	T z;
};

template <class T> struct X<T::template B, T>
{
	typename T::template B<int> y;
	T z;
};

template <class T> struct C
{
	X<T::template B, A> x;
};

int main()
{
	C<A> c;
}
