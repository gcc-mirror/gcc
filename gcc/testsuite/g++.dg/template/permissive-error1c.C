// PR c++/116064
// { dg-additional-options -Wno-error=template-body }
// Like permissive-error1a.C but verify the diagnostics can also
// be downgraded via Wno-error=template-body.

template<class T>
void f() {  // { dg-error "instantiating erroneous template" }
  const int n = 42;
  ++n; // { dg-warning "read-only variable 'n' .-Wtemplate-body." }
       // { dg-message "first error appeared here" "" { target *-*-* } .-1 }
       // { dg-error "read-only variable 'n'\[\n\r\]" "" { target *-*-* } .-2 }
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

int main() {
  f<int>(); // { dg-message "required from here" }
  A<int> a;
  B<int> b;
  b.f();
}
