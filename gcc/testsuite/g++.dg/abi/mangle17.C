// { dg-options "-Wabi" }

enum E { e = 3 };

template <int I> struct S {};

template <int I> void f (S<e + int (3.7)>) {}
template void f<7>(S<e + int (3.7)>);  // { dg-warning "mangle" }

template <int I> void g (S<e + int (3.7)>) {}
template void g<7>(S<e + int (3.7)>); // { dg-warning "mangle" }
