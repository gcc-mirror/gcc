// Build don't link:
// crash test -

// by Paul Burchard <burchard@pobox.com>, Level Set Systems, Inc.
// Copyright (C) 1999 Free Software Foundation

template<class T>
class X {
	class Y : public T {}; // ERROR - invalid base type
	Y y;
};
int main() {
	X<int> x; // ERROR - (instantiated from here)
}

