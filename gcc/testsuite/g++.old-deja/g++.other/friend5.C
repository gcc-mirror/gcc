// { dg-do assemble  }
// submitted by David C Binderman <dcb@pncl.co.uk>

struct A {
	friend bool();		// { dg-error "" } invalid declaration
};
