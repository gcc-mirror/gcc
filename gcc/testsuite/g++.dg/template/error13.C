// PR c++/15227

template<typename> struct A {};

template<typename T> void A<T>::B::foo() {} // { dg-error "" }
