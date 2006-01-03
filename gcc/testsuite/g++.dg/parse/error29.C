// PR c++/25637

struct A { 
  void foo();
  A(); 
  void operator delete(void *);
};
struct B { 
  friend void A::foo() {} // { dg-error "define" }
  friend void A::operator delete(void*) {} // { dg-error "define" }
  friend A::A() {} // { dg-error "define" }
};
