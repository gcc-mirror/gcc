// PR c++/17327

enum E { E0, E1 };
template <class T,class U> class A {};
template <class T> void f(A<E,T>) {}
// We used to issue a "sorry" message.  By using an explicit error
// message below, we make sure that we will not match "sorry".
template void f(A<int,E>); // { dg-error "template-id" }
