// { dg-options "-fshow-column -ansi -pedantic-errors -Wno-long-long" }
// PR c++/25637

struct A { 
  void foo();
  A(); 
  void operator delete(void *);
};
struct B { 
  friend void A::foo() {} // { dg-error "15:cannot define member function 'A::foo' within 'B'" }
  friend void A::operator delete(void*) {} // { dg-error "15:cannot define member function 'A::operator delete' within 'B'" }
  friend A::A() {} // { dg-error "10:cannot define member function 'A::A' within 'B'" }
};
