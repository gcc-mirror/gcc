// { dg-do assemble  }
// Make sure definitions of static members have the right access.

struct A {
protected:
  int i;                        // { dg-error "" } private
  int f ();			// { dg-error "" } 
};

struct B: public A {
  static int A::*p;
  static int (A::*fp)();
};

int A::* B::p = &A::i;         // { dg-error "" } 
int (A::* B::fp)() = &A::f;    // { dg-error "" }

struct C {
  static int A::*p;
  static int (A::*fp)();
};

int A::* C::p = &A::i;		// { dg-error "" } 
int (A::* C::fp)() = &A::f;	// { dg-error "" } 
