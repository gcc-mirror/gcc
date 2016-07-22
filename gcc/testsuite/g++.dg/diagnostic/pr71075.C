// PR c++/71075

template<typename T, int I> struct A {};
template<typename T> void foo(A<T,1>) {}
int main() {
  foo(A<int,2>()); // { dg-error "no matching" }
// { dg-message "template argument .2. does not match .1." "" { target *-*-* } 6 }
}
