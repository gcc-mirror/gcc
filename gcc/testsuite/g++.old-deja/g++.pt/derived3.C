// { dg-do assemble  }

// by Paul Burchard <burchard@pobox.com>, Level Set Systems, Inc.
// Copyright (C) 1999, 2002 Free Software Foundation

template<class T>
class X {
	class Y : public T {};
	Y y;			// { dg-error "" } invalid base type
};
int main() {
	X<int> x;		// { dg-error "" } instantiated
}

