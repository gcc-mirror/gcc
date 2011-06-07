// { dg-options "-Wabi -fabi-version=1" }

enum E { e = 3 };

template <int I> struct S {};

template <int I> void f (S<I + e + int (3.7)>) {} // { dg-warning "mangle" }
template void f<7>(S<7 + e + int (3.7)>); // { dg-message "required" }

template <int I> void g (S<I + e + int (3.7)>) {} // { dg-warning "mangle" }
template void g<7>(S<7 + e + int (3.7)>); // { dg-message "required" }
