// Bug: g++ doesn't handle superfluous parentheses when redeclaring a TYPENAME.
// Build don't link:

typedef int foo;
class A {
    typedef int ((foo));	// gets bogus error - 
};
