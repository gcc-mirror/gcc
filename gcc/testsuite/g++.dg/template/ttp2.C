// Copyright (C) 2001 Free Software Foundation
// Contributed by Kriang Lerdsuwanakij <lerdsuwa@users.sourceforge.net>
// { dg-do compile }

template <class U> struct Alloc {};

template <class T, class U = Alloc<T> > struct Vector {};

template <template <class T, class U = Alloc<T> > class TT>
struct C {
	TT<int> tt;
};

int main()
{
	C<Vector> c;
}
