// PR c++/80412
// { dg-do compile { target c++17 } }

template <typename> struct A;
template <typename> struct B : A < B { , // { dg-error ""  }
