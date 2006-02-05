// { dg-do assemble  }
// PRMS Id: 4375
// Bug: g++ fails to keep track of nested typedefs properly.

class A {
public:
  typedef const char * Ptr;
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

A::A(Ptr string) {		// { dg-bogus "" } 
  s = string;			// { dg-bogus "" } 
}

int main() {
  A a("testing");
  A *a_ptr;
  B b(&a);
  a_ptr = b.get_A();
}
