// Build don't link: 

class A {
public:
  A(int);			// ERROR - referenced below
  A(float);			// ERROR - referenced below
  ~A();
}; // ERROR - synthesized copy ctor

A::A() {		// ERROR - 
}
  
A::A(int) {
}
  
A::~A() {
}
