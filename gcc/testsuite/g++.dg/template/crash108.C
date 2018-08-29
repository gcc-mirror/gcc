// PR c++/50861

template<class T> struct A {A(int b=k(0));}; // { dg-error "not declared" }
 // { dg-error "that depend on a template parameter" "" { target *-*-* } .-1 }
void f(int k){A<int> a;}
