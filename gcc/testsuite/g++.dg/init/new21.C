// PR c++/32251

struct A {
  A();
  void operator delete(void *, ...);
};

void foo () {
  new A; // { dg-warning "deallocation" }
}
