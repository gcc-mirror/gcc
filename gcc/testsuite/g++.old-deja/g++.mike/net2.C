// { dg-do assemble  }
// Here is another program from the net.

class B;

class A {			// { dg-error "" } copy ctor candidate
  private:
    A(B *);			// { dg-error "" } 
  public:
    A(long);			// { dg-error "" } 
};

A a(0); // { dg-error "" } should be ambigious
