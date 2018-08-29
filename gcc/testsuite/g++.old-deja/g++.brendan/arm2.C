// { dg-do assemble  }
// GROUPS passed ARM-compliance
// ARM 9.4 ``There cannot be a static and a nonstatic member function
//	     with the same name and the same argument types.''
//
// The trick is to make sure it's caught with both orders (static,
// then normal, and vice-versa.

class X {
public:
  int foo();            // { dg-message "previous" }
  static int foo();	// error: redeclaration// { dg-error "overloaded" } .*
};

class Y {
public:
   static int foo();    // { dg-message "previous" }
  int foo();		// error: redeclaration// { dg-error "overloaded" } .*
};
