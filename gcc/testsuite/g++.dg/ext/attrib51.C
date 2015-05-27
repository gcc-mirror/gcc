#define FOO __attribute__ (())	// { dg-bogus "" }

void f() throw();		// { dg-message "" }
void f() FOO;			// { dg-error "" }
