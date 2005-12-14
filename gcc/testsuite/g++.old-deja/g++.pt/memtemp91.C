// { dg-do assemble }
// regression test -

// simplified from bug report by Leon Bottou <leonb@research.att.com>
// by Paul Burchard <burchard@pobox.com>, Level Set Systems, Inc.
// Copyright (C) 1999 Free Software Foundation

struct A {
	template <class T>
	struct B {
		T x;
	};
	template <class T>
	struct C : B<T> {
		C() {}
	};
};


