// { dg-do assemble  }
// Bug: g++ tries to call a constructor for a reference.  Doh!

class B;
struct A {
  B & b;
  A (B & x) : b (x) { }		// { dg-bogus "" } 
};				// { dg-bogus "" } 
