// Build don't link: 

class A { // ERROR - synthesized copy ctor
public:
  A(int);			// ERROR - referenced below
  A(float);			// ERROR - referenced below
  ~A();
};

A::A() {		// ERROR - 
}
  
A::A(int) {
}
  
A::~A() {
}
