// g++ 1.36.1 bug 891229_02

// g++ limits the scope of names which are declared as typedef names within
// another type to that other type.

// This conflicts with the (global) scope given to such names by cfront 2.0.

// Cfront 2.0 passes this test.

// Note 2/15/94:  The standard has changed; this limiting of scope is correct
// behavior.  See 9.9.  --jason

// keywords: typedef, nested types, scope

struct foo {
	foo ();
	typedef void (*function_p) (void);
};

function_p fp;		// ERROR - no such type in scope

foo::foo () {}

int main () { return 0; }
