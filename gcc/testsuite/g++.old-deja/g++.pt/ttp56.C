// { dg-do assemble  }

// by Paul Burchard <burchard@pobox.com>, Level Set Systems, Inc.
// Copyright (C) 1999 Free Software Foundation

template<class X>
class A {
};
template<class Y>
class B {
};

template<template<class XX> class AA> // { dg-bogus "" } `template <class XX> template <class X> class A<X>' previously declared here
class C {
	class D {
	};
	D d;
	class E : public B<D> {
	};
	E e;
};

int main() {
	C<A> c; // { dg-bogus "" } redefinition of `template <class XX> template <class X> class A<X>'
}

