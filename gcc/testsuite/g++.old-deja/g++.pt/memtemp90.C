// Build don't link:
// regression test - 

// by Paul Burchard <burchard@pobox.com>, Level Set Systems, Inc.
// Copyright (C) 1999 Free Software Foundation
// related to bug report by Leon Bottou <leonb@research.att.com>

struct A {
	template<class T>
	struct B {
	};
	template<class T>
	struct C {
		B<T> b; // gets bogus error - B is not a template
			// but removing wrapper A gets rid of complaint
			// also, replacing B<T> with A::B<T> also gets rid of complaint
	};
};

