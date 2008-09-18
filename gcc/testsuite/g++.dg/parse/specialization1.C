// PR c++/5723, PR c++/8522
// Origin: Matthias Kleinmann, Peter Karl Mueller <peter.karl.mueller@gmx.de>
// { dg-do compile }

template <typename T> class A;
template <typename T> class A<T>::B; // { dg-error "declaration" "err" }
// { dg-warning "declaration" "warn" { target *-*-* } 6 }
