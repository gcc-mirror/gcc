// g++ 1.36.1 bug 891230_01

// g++ gives typedefs which are nested within class declarations a scope
// which is local to the class declaration itself.  This causes examples
// like the following to get compile-time errors.

// Cfront 2.0 passes this test.

// keywords: typedef, nested types, scope

struct foo {

    typedef foo* foo_p;
    void member (foo_p);
};

void foo::member (foo_p p) {	// gets bogus errors
}

int main () { return 0; }
