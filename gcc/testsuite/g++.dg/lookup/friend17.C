// PR c++/69657
// { dg-options "-Wpedantic" }

namespace N {
  struct A {
    friend void f(A);
  };
}
void N::f(A) { }		// { dg-warning "declared" }
