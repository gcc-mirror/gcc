// Test typeof template argument substitution

// Copyright (C) 2001 Free Software Foundation
// Contributed by Kriang Lerdsuwanakij <lerdsuwa@users.sourceforge.net>
// { dg-do compile }
// { dg-options "" }

template <class T> struct A {
	void f() {}
	void g(T* t) {
		A<typeof(t)> a;
		a.f();
	}
};

int main()
{
	A<int> a;
	int b;
	a.g(&b);
}
