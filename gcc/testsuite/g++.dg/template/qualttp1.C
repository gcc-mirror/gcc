// Copyright (C) 2001 Free Software Foundation
// Contributed by Kriang Lerdsuwanakij <lerdsuwa@users.sourceforge.net>
// { dg-do compile }

struct A
{
	template <class T> class B {};
};

template <class T> struct X
{
};

template <class T> struct C
{
	X<T::template B> x; // { dg-error "type" }
};

int main()
{
	C<A> c;
}
