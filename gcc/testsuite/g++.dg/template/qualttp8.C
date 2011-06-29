// Copyright (C) 2001 Free Software Foundation
// Contributed by Kriang Lerdsuwanakij <lerdsuwa@users.sourceforge.net>
// { dg-do compile }

template <template <class> class TT> class C {
};

template <class T> struct D {
	C<T::template B> c; // { dg-error "context" }
};

struct E {
	private:
	template <class T> class B {}; // { dg-error "private" }
};

D<E> d; // { dg-message "required" }
