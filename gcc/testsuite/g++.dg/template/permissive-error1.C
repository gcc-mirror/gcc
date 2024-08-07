// PR c++/116064
// { dg-additional-options -fpermissive }

template<class T>
void f() {
  const int n = 42;
  ++n; // { dg-warning "read-only" }
}

template<class T>
struct A {
  void f(typename A::type); // { dg-warning "does not name a type" }
};

template<class T>
struct B {
  void f() {
    this->g(); // { dg-warning "no member" }
  }
};
