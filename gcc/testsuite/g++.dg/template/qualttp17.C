// Copyright (C) 2001 Free Software Foundation
// Contributed by Kriang Lerdsuwanakij <lerdsuwa@users.sourceforge.net>
// { dg-do compile }
// { dg-options "-fno-inline -fabi-version=1" }

struct A
{
	template <class T> class B {};
};

template <template <class> class TT> struct X
{
};

template <class T> void f(X<T::template B>)
{
}

int main()
{
	X<A::B> x;
	f<A>(x);
}

// { dg-final { scan-assembler "\n_?_Z1fI1AEv1XIN1T1BEE\[: \t\n\]" } }
