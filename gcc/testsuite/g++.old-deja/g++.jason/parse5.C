// { dg-do assemble  }
// Bug: foo (bar) should be a declaration of a static data member, not a
// function; it's getting caught by the rules for constructors.

typedef int foo;
typedef int bar;
struct A {
  static foo (bar);		// { dg-bogus "" } 
};

int i = A::bar;			// { dg-bogus "" } 
int (*fp)(bar) = A::foo;	// { dg-error "" } 
