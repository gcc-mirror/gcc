// { dg-options "-Wabi -fabi-version=1" }

template <typename Q>
void f (typename Q::X) {} // { dg-warning "mangle" }

struct S {
  typedef int X;
};

template void f<S> (int); // { dg-message "instantiated" }
