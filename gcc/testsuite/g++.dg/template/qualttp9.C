// Copyright (C) 2001 Free Software Foundation
// Contributed by Kriang Lerdsuwanakij <lerdsuwa@users.sourceforge.net>
// { dg-do compile }

template <template <class> class TT> class C {
};

template <class T> struct D : T {
	C<T::template B> c;
};

struct E {
	protected:
	template <class T> class B {};
};

D<E> d;
