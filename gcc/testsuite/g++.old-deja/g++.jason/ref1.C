// Bug: g++ tries to call a constructor for a reference.  Doh!
// Build don't link:

class B;
struct A {
  B & b;
  A (B & x) : b (x) { }		// gets bogus error - 
};				// gets bogus error - 
