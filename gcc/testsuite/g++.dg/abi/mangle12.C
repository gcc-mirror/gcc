// { dg-options "-Wabi -fabi-version=1" }

template <template <typename> class Q>
void f (typename Q<int>::X) {} // { dg-warning "mangle" }

template <typename Q>
struct S {
  typedef int X;
};

template void f<S> (int); 
