// { dg-options "-Wabi" }

template <template <typename> class Q>
void f (typename Q<int>::X) {}

template <typename Q>
struct S {
  typedef int X;
};

template void f<S> (int); // { dg-warning "mangle" }
