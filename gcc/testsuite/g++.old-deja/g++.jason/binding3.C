// { dg-do assemble  }
// Bug: g++ fails to clear out the IDENTIFIER_CLASS_VALUEs of various names
// after a class definition.

struct A {
  typedef double T;
  virtual T f () = 0;
};

class B {
  B (const B&);
  void operator=(const B&);
public:
  B ();
  typedef void * T;
};

struct C : public A {
  T f ();			// { dg-bogus "" } 
};
