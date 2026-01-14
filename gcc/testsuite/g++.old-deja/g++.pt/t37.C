// { dg-do compile  }

class A { // { dg-message "A::A\\\(const A&\\\)" } synthesized copy ctor
  // { dg-message "'class A' defined here" "note" { target *-*-* } .-1 }
public:
  A(int);			// { dg-message "A::A\\\(int\\\)" }
  A(float);			// { dg-message "A::A\\\(float\\\)" }
  ~A();
};

A::A() {		// { dg-error "no declaration matches" } 
}
  
A::A(int) {
}
  
A::~A() {
}
