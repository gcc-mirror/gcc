// { dg-do assemble  }
// Bug: g++ doesn't handle superfluous parentheses when redeclaring a TYPENAME.

typedef int foo;
class A {
    typedef int ((foo));	// { dg-bogus "" } 
};
