// Copyright (C) 2001 Free Software Foundation
// Contributed by Kriang Lerdsuwanakij <lerdsuwa@users.sourceforge.net>
// { dg-do compile }

template <template <class> class TT> class C {
};

template <class T> struct D {
	template <class U> class B {};
	C<D<T>::template B> c;
};

D<int> d;
