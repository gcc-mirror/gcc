// PR c++/116064
// { dg-additional-options -Wno-template-body }
// Like permissive-error1a.C but verify -Wno-template-body suppresses
// diagnostics.

template<class T>
void f() {  // { dg-error "instantiating erroneous template" }
  const int n = 42;
  ++n; // { dg-message "first error appeared here" "" { target *-*-* } }
       // { dg-error "read-only variable 'n'\[\n\r\]" "" { target *-*-* } .-1 }
}

template<class T>
struct A {
  void f(typename A::type);
};

template<class T>
struct B {
  void f() {
    this->g();
  }
};

int main() {
  f<int>(); // { dg-message "required from here" }
  A<int> a;
  B<int> b;
  b.f();
}
