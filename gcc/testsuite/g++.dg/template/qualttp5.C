// Copyright (C) 2001 Free Software Foundation
// Contributed by Kriang Lerdsuwanakij <lerdsuwa@users.sourceforge.net>
// { dg-do compile }

template <class U> struct A
{
	template <class T> class B {}; // { dg-message "candidate is" }
};

template <template <class> class TT> void f()
{
	TT<int> y;
	y = 0; // { dg-error "no match" }
}

template <class T> struct C
{
	void g() { f<A<T>::template B>(); } // { dg-message "instantiated" }
};

int main()
{
	C<int> c;
	c.g(); // { dg-message "instantiated" }
}
