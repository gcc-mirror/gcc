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

struct C
{
	X<A::template B> x; // { dg-error "template|forbid" }
};

int main()
{
	C c;
}
