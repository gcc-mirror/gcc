// { dg-options "-Wabi -fabi-version=1" }

template <typename Q>
void f (typename Q::X) {}

struct S {
  typedef int X;
};

template void f<S> (int); // { dg-warning "mangle" }
