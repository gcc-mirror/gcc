// { dg-options "-Wabi" }

template <typename Q>
void f (typename Q::X) {}

struct S {
  typedef int X;
};

template void f<S> (int); // { dg-warning "mangle" }
