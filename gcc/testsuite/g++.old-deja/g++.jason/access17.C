// Make sure definitions of static members have the right access.
// Build don't link:

struct A {
protected:
  int i;                        // ERROR - private
  int f ();			// ERROR - 
};

struct B: public A {
  static int A::*p;
  static int (A::*fp)();
};

int A::* B::p = &A::i;
int (A::* B::fp)() = &A::f;

struct C {
  static int A::*p;
  static int (A::*fp)();
};

int A::* C::p = &A::i;		// ERROR - 
int (A::* C::fp)() = &A::f;	// ERROR - 
