// PR c++/50861

template<class T> struct A {A(int b=k(0));}; // { dg-error "parameter|argument" }
void f(int k){A<int> a;} // // { dg-message "declared" }
// { dg-message "note" "note" { target *-*-* } 3 }
