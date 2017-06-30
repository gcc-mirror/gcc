// { dg-do compile  }

class A { // { dg-message "A::A" } synthesized copy ctor
  // { dg-message "defined here" "note" { target *-*-* } .-1 }
public:
  A(int);			// { dg-message "A::A" }
  A(float);			// { dg-message "A::A" }
  ~A();
};

A::A() {		// { dg-error "" } 
}
  
A::A(int) {
}
  
A::~A() {
}
