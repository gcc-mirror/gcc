// PRMS Id: 4375
// Bug: g++ fails to keep track of nested typedefs properly.
// Build don't link:

class A {
public:
  typedef char * Ptr;
  Ptr s;
  Ptr get_string();
  A(Ptr string); // { s = string; };
};

class B {
public:
  typedef A * Ptr;
  Ptr a;
  Ptr get_A();
  B(Ptr a_ptr);
};

A::A(Ptr string) {		// gets bogus error - 
  s = string;			// gets bogus error - 
}

int main() {
  A a("testing");
  A *a_ptr;
  B b(&a);
  a_ptr = b.get_A();
}
