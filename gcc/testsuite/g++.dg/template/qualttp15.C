// Copyright (C) 2001 Free Software Foundation
// Contributed by Kriang Lerdsuwanakij <lerdsuwa@users.sourceforge.net>
// { dg-do compile }

struct A
{
	template <class T> class B {};
};

template <template <class> class TT> struct X
{
	TT<int> y;
};

template <class T> struct X<T::template B>
{	// { dg-error "previous" }
	T z;
};

template <class T> struct X<T::template B> // { dg-error "redefinition" }
{	
	T z;
};

template <class T> struct C
{
	X<T::template B> x;
};

int main()
{
	C<A> c;
}
