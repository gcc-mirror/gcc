// Build don't link:
// crash test -

// by Paul Burchard <burchard@pobox.com>, Level Set Systems, Inc.
// Copyright (C) 1999, 2002 Free Software Foundation

template<class T>
class X {
	class Y : public T {};
	Y y;			// ERROR - invalid base type
};
int main() {
	X<int> x;		// ERROR - instantiated
}

