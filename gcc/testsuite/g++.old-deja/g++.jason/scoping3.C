// Bug: g++ does not grok nested types very well.
// Build don't link:

class A {
  class B;
  friend class B;
  class B { };			// gets bogus error - 
};
