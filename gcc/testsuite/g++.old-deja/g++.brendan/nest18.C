// { dg-do assemble  }
// GROUPS passed nested-classes
// This is the first line of file ||t2.C||.

// This code demonstrates what appears to be a bug with nested types.
// In C++, nested typedefs are not supposed to be visible outside
// their class scopes but they apparently are in gcc 2.4.5.  This code
// compiles fine in AT&T cfront 3.0.1, but will not compile with gcc.

// If this class does not precede String, then the code will compile.

class Another {
public:
    typedef int Length;
};

// If String does not define typedef int Length, then the code will
// compile.

class String {
public:
    typedef int Length;		// remove this and it will compile fine

    int foo(Length length) const;
};

int String::foo(Length length) const {
    return length;
}

// File ||t2.C|| ends here.
