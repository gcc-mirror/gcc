// Bug: foo (bar) should be a declaration of a static data member, not a
// function; it's getting caught by the rules for constructors.
// Build don't link:

typedef int foo;
typedef int bar;
struct A {
  static foo (bar);		// gets bogus error
};

int i = A::bar;			// gets bogus error
int (*fp)(bar) = A::foo;	// ERROR - 
