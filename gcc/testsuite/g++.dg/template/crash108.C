// PR c++/50861

template<class T> struct A {A(int b=k(0));}; // { dg-error "arguments" }
void f(int k){A<int> a;} // // { dg-error "parameter|declared" }
// { dg-message "note" { target *-*-* } 3 }
