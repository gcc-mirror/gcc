// { dg-do assemble  }

class A { // { dg-error "" } synthesized copy ctor
public:
  A(int);			// { dg-error "" } referenced below
  A(float);			// { dg-error "" } referenced below
  ~A();
};

A::A() {		// { dg-error "" } 
}
  
A::A(int) {
}
  
A::~A() {
}
