// Copyright (C) 2001 Free Software Foundation
// Contributed by Kriang Lerdsuwanakij <lerdsuwa@users.sourceforge.net>

template <class U> struct A
{
	template <class V> struct AA {
		template <class T> struct B {
			int i;
			B() : i(1) {}
		};
	};
};

template <template <class> class TT> struct X
{
	TT<int> y;
};

template <class T, class U> struct C
{
	X<T::template AA<U>::template B> x;
};

int main()
{
	C<A<char>, int> c;
}
