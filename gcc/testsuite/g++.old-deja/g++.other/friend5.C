// Build don't link:

// submitted by David C Binderman <dcb@pncl.co.uk>

// crash test - XFAIL *-*-*

struct A {
	friend bool();
};
