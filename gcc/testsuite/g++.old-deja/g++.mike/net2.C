// { dg-do assemble  }
// Here is another program from the net.

class B;

class A {			// { dg-message "note" } copy ctor candidate
  private:
    A(B *);			// { dg-message "note" } 
  public:
    A(long);			// { dg-message "note" } 
};

A a(0); // { dg-error "ambiguous" }
// { dg-message "candidate" "candidate note" { target *-*-* } 13 }
