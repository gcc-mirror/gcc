// { dg-do assemble  }
// Bug: g++ does not grok nested types very well.

class A {
  class B;
  friend class B;
  class B { };			// { dg-bogus "" } 
};
