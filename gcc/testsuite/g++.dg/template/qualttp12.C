// Copyright (C) 2001 Free Software Foundation
// Contributed by Kriang Lerdsuwanakij <lerdsuwa@users.sourceforge.net>
// { dg-do link }

struct A
{
	template <class T> class B {};
};

template <template <class> class TT> void f()
{
	TT<int> y;
}

template <class T> struct C
{
	void g() { f<T::template B>(); }
};

int main()
{
	C<A> c;
	c.g();
}
