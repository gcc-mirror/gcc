// Build don't link: 
// GROUPS passed ARM-compliance
// ARM 9.4 ``There cannot be a static and a nonstatic member function
//	     with the same name and the same argument types.''
//
// The trick is to make sure it's caught with both orders (static,
// then normal, and vice-versa.

class X {
public:
   int foo();
  static int foo();	// error: redeclaration// ERROR - .*
};

class Y {
public:
   static int foo();
  int foo();		// error: redeclaration// ERROR - .*
};
