// { dg-do compile  }

// by Paul Burchard <burchard@pobox.com>, Level Set Systems, Inc.
// Copyright (C) 1999, 2002 Free Software Foundation

template<class T>
class X {
	class Y : public T	// { dg-error "base type .* fails to be" }
	{
	};
	Y y;			// { dg-message "required" }
};
int main() {
	X<int> x;		// { dg-message "required" }
}

