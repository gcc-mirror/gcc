// Bug: g++ allows two different meanings of a name in the same scope.

typedef int foo;		// ERROR - 
struct A {
  A (foo);
  int foo ();			// ERROR - foo already used in scope
};
